module vayne.source.parser;


import std.algorithm;
import std.array;
import std.conv;
import std.format;
import std.string;

import vayne.ast.node;
import vayne.source.compress;
import vayne.source.context;
import vayne.source.lexer;
import vayne.source.source;
import vayne.source.token;


struct ParserOptions {
	CompressOptions compress;
}


class ParserException : Exception {
	this(SourceLoc loc, string msg) {
		super(msg);

		this.loc = loc;
	}

	SourceLoc loc;
}


Node parse(ref SourceManager mgr, uint id, ParserOptions options) {
	return Parser(mgr, id, options)();
}


private struct Parser {
	this(ref SourceManager mgr, uint id, ParserOptions options) {
		source_ = mgr.get(id);
		mgr_ = &mgr;
		options_ = options;
	}

	Node opCall() {
		return parse();
	}

private:
	Node parse() {
		string errors;

		try {
			return parse(source_);
		} catch(Exception error) {
			if (auto ctxError = cast(ContextException)error) {
				errors = format("%s: %s", mgr_.loc(ctxError.loc), error.msg);
			} else if (auto parserError = cast(ParserException)error) {
				errors = format("%s: %s", mgr_.loc(parserError.loc), error.msg);
			} else {
				errors = error.msg;
			}
		}

		throw new Exception(errors);
	}

	Node parse(Source source) {
		auto context = new Context(source);

		insert_ = new StatementBlock(Token(context.loc));

		const end = source.buffer.length - 2;

		while (context.cursor < end) {
			auto remaining = context.remaining();
			auto indexOpen = remaining.indexOf("{{");
			if (indexOpen == -1)
				break;

			escaper(context, remaining[0..indexOpen]);
			context.advance(indexOpen);

			const triple = ((indexOpen + 1 < remaining.length) && (remaining[indexOpen + 2] == '{'));
			const open = triple ? "{{{" : "{{";
			const contentStart = indexOpen + open.length;
			const close = triple ? "}}}" : "}}";
			auto indexClose = remaining.indexOf(close, contentStart);
			while (indexClose != -1) {
				if (balancedQuotes(remaining[contentStart..indexClose]))
					break;

				indexClose = remaining.indexOf(close, indexClose + close.length);
			}

			if (indexClose == -1)
				throw new ParserException(context.loc, format("missing '%s' to close tag '%s'", close, open));

			context.advance(close.length);
			indexClose -= contentStart;

			try {
				compile(context, source.buffer[context.cursor..context.cursor + indexClose], open, close);
			} catch (ExprParserException error) {
				throw new ParserException(context.loc, error.msg);
			}

			context.advance(indexClose + close.length);
		}
		context.expectClosed();

		if (context.cursor > 0) {
			escaper(context, context.remaining());
		} else {
			escaper(context, source.buffer);
		}

		context.advance(context.remaining.length);

		/*foreach(name, values; settings) {
			if (values.length > 1)
				throw new ParserException(("missing pop for '", name, "' - stack is not empty"), context);
		}*/

		assert(insertStack_.empty);

		return new Module(Token(context.loc), [ insert_ ]);
	}

	void compile(Context context, string content, string tagOpen, string tagClose) {
		if (content.length > 0) {
			auto tag = content[0..1];
			switch(tag) {
			case "*":
				iterate(context, content);
				break;
			case "/":
				close(context, content);
				break;
			case "?":
				 conditional(context, content);
				 break;
			case ":":
				orElse(context, content);
				break;
			case "~":
				translate(context, content, tagOpen.length == 2);
				break;
			case ";":
				meta(context, content);
				break;
			case "!":
				break;
			case "#":
				define(context, content);
				break;
			case "@":
				withs(context, content);
				break;
			case "&":
				break;
			default:
				interpolate(context, content, tagOpen.length == 2);
				break;
			}
		}
	}

	void iterate(Context context, string content) {
		context.open(content[0..1], content[1..$]);
		content = content[1..$].strip();

		auto loopStmt = parseLoop(Source(source_.id, source_.parent, content), context.loc);
		auto bodyBlock = create!StatementBlock(Token(context.loc));
		loopStmt.children[2] = bodyBlock;
		insert_.children ~= loopStmt;

		insertStack_ ~= insert_;
		insert_ = bodyBlock;
	}

	void close(Context context, string content) {
		context.close();

		insert_ = insertStack_.back;
		insertStack_.popBack;
	}

	void conditional(Context context, string content) {
		context.open(content[0..1], content[1..$]);
		content = content[1..$].strip;

		auto ifStmt = create!IfStatement(Token(context.loc), parseExpr(Source(source_.id, source_.parent, content), context.loc), create!StatementBlock(Token(context.loc)), null);
		insert_.children ~= ifStmt;

		insertStack_ ~= insert_;
		insert_ = cast(StatementBlock)ifStmt.children[1];
	}

	void orElse(Context context, string content) {
		context.expectOpen("?", ":");
		content = content[1..$].strip;

		insert_ = insertStack_.back;
		insertStack_.popBack;

		auto ifStmt = cast(IfStatement)insert_.children.back;
		assert(ifStmt !is null);

		while (ifStmt.children[2] !is null) {
			ifStmt = cast(IfStatement)ifStmt.children[2];
			if (ifStmt is null)
				throw new ParserException(context.loc, "unexpected else statement");
		}

		if (content.length) {
			assert(ifStmt.children[2] is null);

			auto elseIfStmt = create!IfStatement(Token(context.loc), parseExpr(Source(source_.id, source_.parent, content), context.loc), create!StatementBlock(Token(context.loc)), null);
			ifStmt.children[2] = elseIfStmt;

			insertStack_ ~= insert_;
			insert_ = cast(StatementBlock)elseIfStmt.children[1];
		} else {
			assert(ifStmt.children[2] is null);

			auto elseBlock = create!StatementBlock(Token(context.loc));
			ifStmt.children[2] = elseBlock;

			insertStack_ ~= insert_;
			insert_ = cast(StatementBlock)ifStmt.children[2];
		}
	}

	void meta(Context context, string content) {
		content = content[1..$].strip;
		auto values = content.splitter(':');
		auto type = values.front.strip;
		values.popFront;

		switch (type) {
		case "src":
			auto id = values.front.to!uint;
			values.popFront;

			auto line = values.front.to!uint;
			context.loc = SourceLoc(id, line, 0);
			break;
		default:
			throw new ParserException(context.loc, format("unknown meta type '%s'", type));
		}
	}

	void define(Context context, string content) {
		//insert_.children ~= new Output(Token(context.loc), parseExpr(Source(source_.id, source_.parent, content), context.loc));
	}

	void withs(Context context, string content) {
		context.open(content[0..1], content[1..$]);
		content = content[1..$].strip;

		auto withStmt = parseWith(Source(source_.id, source_.parent, content), context.loc);
		auto bodyBlock = create!StatementBlock(Token(context.loc));
		withStmt.children[$-1] = bodyBlock;
		insert_.children ~= withStmt;

		insertStack_ ~= insert_;
		insert_ = bodyBlock;
	}

	Node escapeHTML(Node text, SourceLoc loc) {
		auto escape = create!Identifier(Token(Token.Kind.Identifier, "__escape", loc));
		auto html = cast(Node)create!Constant(Token("html", Token.LiteralKind.String, 0, 0, loc));

		return cast(Node)create!FunctionCall(Token(loc), escape, [ text, html ]);
	}

	void translate(Context context, string content, bool autoEscape) {
		content = content[1..$].strip;

		auto args = cast(Node[])parseExprList(Source(source_.id, source_.parent, content), context.loc);
		auto translateFunc = create!Identifier(Token(Token.Kind.Identifier, "__translate", context.loc));

		auto translated = cast(Node)create!FunctionCall(Token(context.loc), translateFunc, args);
		if (autoEscape)
			translated = escapeHTML(translated, context.loc);
		insert_.children ~= create!Output(Token(context.loc), translated);
	}

	void interpolate(Context context, string content, bool autoEscape) {
		auto args = cast(Node[])parseExprList(Source(source_.id, source_.parent, content), context.loc);
		foreach (arg; args) {
			if (autoEscape)
				arg = escapeHTML(arg, context.loc);
			insert_.children ~= create!Output(Token(context.loc), arg);
		}
	}

	void escaper(Context context, string content) {
		if (content.length) {
			if (options_.compress)
			   content = compress(content, options_.compress);
			content = content.replace("\r", "");
		}

		if (content.length)
			insert_.children ~= create!Output(Token(context.loc), create!Constant(Token(content, Token.LiteralKind.String, 0, 0, context.loc)));
	}

	Source source_;

	StatementBlock insert_;
	StatementBlock[] insertStack_;

	SourceManager* mgr_;
	ParserOptions options_;
}


class ExprParserException : Exception {
	this(Token tok, string msg) {
		super(msg);
		this.tok = tok;
	}

	Token tok;
}


auto parseExpr(Source source, SourceLoc loc) {
	auto parser = ExprParser(source, loc);
	scope (success) parser.ensureEndOfInput();
	return parser.parseExpression();
}


auto parseExprList(Source source, SourceLoc loc) {
	auto parser = ExprParser(source, loc);
	scope (success) parser.ensureEndOfInput();
	return parser.parseExpressionList();
}


auto parseLoop(Source source, SourceLoc loc) {
	auto parser = ExprParser(source, loc);
	scope (success) parser.ensureEndOfInput();
	return parser.parseLoopStatement(loc);
}


auto parseWith(Source source, SourceLoc loc) {
	auto parser = ExprParser(source, loc);
	scope (success) parser.ensureEndOfInput();
	return parser.parseWithStatement(loc);
}


private struct ExprParser {
	this(Source source, SourceLoc loc) {
		lexer_ = Lexer(source, loc);

		warmUp();
	}

	Expression parseExpression() {
		auto start = tok_;

		if (auto left = parseExpressionPrimary()) {
			auto expr = parseBinaryOp(left, 0);
			if (!expr)
				expr = left;

			if (auto cond = parseConditional(expr))
				return create!Expression(start, cond);
			return create!Expression(start, expr);
		}
		return null;
	}

	Expression[] parseExpressionList() {
		if (auto expr = parseExpression) {
			Expression[] exprs;
			exprs ~= expr;

			while (tok_.sep(',')) {
				eat();
				if (auto next = parseExpression()) {
					exprs ~= next;
					continue;
				}
				throw new ExprParserException(tok_, format("expected an expression following ',', not '%s'", tok_));
			}
			return exprs;
		}
		return null;
	}

	WithStatement parseWithStatement(SourceLoc loc) {
		if (auto expr = parseWithExpression) {
			WithExpression[] exprs;
			exprs ~= expr;

			while (tok_.sep(',')) {
				eat();
				if (auto next = parseWithExpression()) {
					exprs ~= next;
					continue;
				}
				throw new ExprParserException(tok_, format("expected a with expression following ',', not '%s'", tok_));
			}
			return create!WithStatement(Token(loc), cast(Node[])exprs, null);
		}
		return null;
	}

	WithExpression parseWithExpression() {
		if (auto expr = parseExpression()) {
			Token name;
			if (tok_.keyword(Token.KeywordKind.As)) {
				eat();
				if (!tok_.ident())
					throw new ExprParserException(tok_, format("expected an identifier following 'as', not '%s'", tok_));
				name = eat();
			}
			return create!WithExpression(expr.tok, expr, name);
		}
		return null;
	}

	LoopStatement parseLoopStatement(SourceLoc loc) {
		if (!tok_.ident)
			throw new ExprParserException(tok_, format("expected an identifier, not '%s'", tok_));

		Token key = eat();
		Token name;
		if (tok_.sep(',')) {
			eat();
			if (!tok_.ident)
				throw new ExprParserException(tok_, format("expected an identifier, not '%s'", tok_));
			name = eat();
			if (!tok_.sep(';'))
				throw new ExprParserException(tok_, format("expected ';', not '%s'", tok_));
			eat();
		} else if (tok_.sep(';')) {
			eat();
			swap(key, name);
		} else {
			throw new ExprParserException(tok_, format("expected ';' or ',' followed by an identifier, not '%s'", tok_));
		}

		Expression obj;
		Expression end;

		obj = parseExpression();
		if (!obj)
			throw new ExprParserException(tok_, format("expected an expression, not '%s'", tok_));

		if (tok_.name == "..") {
			eat();
			end = parseExpression();
			if (!end)
				throw new ExprParserException(tok_, format("expected an expression, not '%s'", tok_));
		}

		return create!LoopStatement(Token(loc), key, name, obj, end, null);
	}

	Node parseExpressionPrimary() {
		if (auto lexpr = parseExpressionPrimarySimple()) {
			if (tok_.sep()) {
				if (auto primary = parseSuffixOp(lexpr)) {
					while (!tok_.eoi() && tok_.sep()) {
						if (auto suffix = parseSuffixOp(primary)) {
							primary = suffix;
							continue;
						}
						return primary;
					}
					return primary;
				}
			}
			return lexpr;
		}
		return null;
	}

	Node parseExpressionPrimarySimple() {
		switch (tok_.kind) with (Token.Kind) {
		case Identifier:
			return parseIdentifierExpr();
		case Separator:
			if (tok_.sep('(')) {
				auto start = eat();
				auto expr = parseExpression();
				if (!expr)
					throw new ExprParserException(tok_, format("expected an expression, not '%s'", tok_));

				close(')');
				return create!Expression(start, expr, true);
			}

			if (auto expr = parseUnaryOp())
				return expr;
			break;
		case Literal:
			return parseLiteralExpr();
		case Keyword:
			switch (tok_.kindKeyword) with (Token.KeywordKind) {
			case True:
				return create!Constant(eat());
			case False:
				return create!Constant(eat());
			default:
				throw new ExprParserException(tok_, format("unexpected '%s'", tok_));
			}
		case Undefined:
		case EndOfInput:
		default:
			break;
		}

		return null;
	}

	Node parseArrayConstructor() {

		return null;
	}

	Node parseIdentifierExpr() {
		assert(tok_.ident());
		return create!Identifier(eat());
	}

	bool isUnaryOp(Token tok) const {
		if (tok.sep()) {
			switch (tok.length) {
			case 1:
				switch(tok.front) {
				case '-':
				case '+':
				case '!':
				case '~':
				case '^':
				case '*':
					return true;
				default:
					break;
				}
				break;
			default:
				break;
			}
		}
		return false;
	}

	UnaryOp parseUnaryOp() {
		assert(tok_.sep());
		if (isUnaryOp(tok_)) {
			auto op = eat();
			auto expr = parseExpressionPrimary();
			if (!expr)
				throw new ExprParserException(tok_, format("expected an expression, not '%s'", tok_));
			return create!UnaryOp(op, expr);
		}
		return null;
	}

	Node parseSuffixOp(Node expr) {
		switch(tok_.name) {
		case "[":
			auto op = eat();
			auto index = parseExpression();
			Expression end;
			if (!index)
				throw new ExprParserException(tok_, format("expected an index expression, not '%s'", tok_));

			if (tok_.sep("..")) {
				eat();
				end = parseExpression;
				if (!end)
					throw new ExprParserException(tok_, format("expected an expression following '..', not '%s'", tok_));
			}
			close(']');

			if (end is null)
				return create!IndexOp(op, expr, index);
			return create!SliceOp(op, expr, index, end);
		case ".":
			auto op = eat();
			if (!tok_.ident())
				throw new ExprParserException(tok_, format("expected an identifier following '.', not '%s'", tok_));
			auto ident = eat();
			return create!DispatchOp(op, expr, ident);
		case "(":
			auto op = eat();

			Node[] args;

			if (!tok_.sep(')')) {
				while (true) {
					if (auto arg = parseExpression()) {
						args ~= arg;
						if (tok_.sep(')'))
							break;
						if (tok_.sep(',')) {
							eat();
							continue;
						}

						throw new ExprParserException(tok_, format("expected ')' or ',' not '%s'", tok_));
					}
					break;
				}
			}

			close(')');
			return create!FunctionCall(op, expr, args);
		case "!":
			auto op = eat();

			Node[] args;
			if (tok_.literal()) {
				args ~= parseLiteralExpr();
			} else if (tok_.ident()) {
				args ~= parseIdentifierExpr();
			} else {
				throw new ExprParserException(tok_, format("expected a literal or an identifier following '!', not '%s'", tok_));
			}

			return create!FunctionCall(op, expr, args);
		default:
			break;
		}
		return null;
	}

	Constant parseLiteralExpr() {
		assert(tok_.literal());
		return create!Constant(eat());
	}

	Node parseUnaryExpr() {
		return null;
	}

	enum OperatorPriority {
		Logic       = 10,
		Arithmetic  = 20,
		Algebraic   = 30,
		Bitwise     = 40,
	}

	size_t isBinaryOp(string name) const {
		switch(name.front) with (OperatorPriority) {
		case '&':
		case '|':
			return (name.length == 1) ? Bitwise : Logic;
		case '>':   // >, >=, >>, >>=
		case '<':   // <, <=, <<, <<=
			return ((name.length == 1) || (name[1] == '=')) ? Logic : Bitwise;
		case '^':   // ^, ^=, ^^, ^^=
			return ((name.length == 1) || (name[1] == '=')) ? Bitwise : Algebraic;
		case '+':
		case '-':
			return (name.length == 1) ? Arithmetic : 0;
		case '~':
		case '*':
		case '/':
		case '%':
			return (name.length == 1) ? Algebraic : 0;
		case '=':
			return (name.length == 2) ? Logic : 0;
		case '!':
			return (name.length == 2) ? Logic : 0;
		default:
			if ((name.length == 2) && (name == "in"))
				return Bitwise;
			break;
		}
		return 0;
	}

	Node parseBinaryOp(Node left, size_t prioExpr) {
		while (true) {
			auto prio = isBinaryOp(tok_.name);
			if (!prio || (prio < prioExpr))
				return left;
			auto op = eat();
			auto right = parseExpressionPrimary();
			if (!right)
				return null;
			auto prioNext = isBinaryOp(tok_.name);
			if (prio < prioNext) {
				right = parseBinaryOp(right, prio + 1);
				if (!right)
					return null;
			}

			left = create!BinaryOp(op, left, right);
		}
	}

	Node parseConditional(Node expr) {
		if (tok_.sep('?')) {
			auto op = eat();
			Node trueCase = parseExpression();
			Node falseCase = null;
			if (trueCase) {
				if (tok_.sep(':')) {
					eat();
					falseCase = parseExpression();
					if (!falseCase)
						throw new ExprParserException(tok_, format("expected an expression, not '%s'", tok_));
				} else {
					throw new ExprParserException(tok_, format("expected ':', not '%s'", tok_));
				}
			} else {
				throw new ExprParserException(tok_, format("expected an expression, not '%s'", tok_));
			}

			return create!ConditionalExpression(op, expr, trueCase, falseCase);
		}

		return null;
	}

	void warmUp() {
		tok_ = lexer_.front;
		lexer_.popFront;

		if (!lexer_.empty) {
			foreach (ref atok; ahead_) {
				atok = lexer_.front;
				lexer_.popFront;
				if (lexer_.empty)
					return;
			}
		}
	}

	Token eat() {
		if (!tok_.eoi) {
			foreach (i; 1..behind_.length)
				behind_[i] = behind_[i - 1];

			behind_[0] = tok_;
			tok_ = ahead_[0];
			foreach (i; 1..ahead_.length)
				ahead_[i - 1] = ahead_[i];

			if (!lexer_.empty) {
				ahead_[$ - 1] = lexer_.front;
				lexer_.popFront;
			}

			return behind_[0];
		}

		return tok_;
	}

	void open(char separator, Token by) {
		if (!tok_.sep(separator)) {
			throw new ExprParserException(tok_, format("expected '%s' following '%s', not '%s'", separator, by, tok_));
		} else {
			eat();
		}
	}

	void close(char separator) {
		if (!tok_.sep(separator)) {
			throw new ExprParserException(tok_, format("expected '%s', not '%s'", separator, tok_));
		} else {
			eat();
		}
	}

	void ensureEndOfInput() {
		if (!tok_.eoi)
			throw new ExprParserException(tok_, format("unexpected '%s'", tok_));
	}

private:
	Token tok_;
	Token[1] behind_;
	Token[1] ahead_;
	Lexer lexer_;
}
