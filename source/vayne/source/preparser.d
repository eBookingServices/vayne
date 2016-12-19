module vayne.source.preparser;


import std.array;
import std.format;
import std.string;
import std.traits;

import vayne.source.context;
import vayne.source.lexer;
import vayne.source.mime;
import vayne.source.source;
import vayne.source.token;


struct PreParserOptions {
	bool lineNumbers;
}


class PreParserException : Exception {
	this(SourceLoc loc, string msg) {
		super(msg);

		this.loc = loc;
	}

	SourceLoc loc;
}


string preparse(ref SourceManager mgr, uint id, PreParserOptions options) {
	return PreParser(mgr, id, options)();
}


private struct PreParser {
	this(ref SourceManager mgr, uint id, PreParserOptions options) {
		mgr_ = &mgr;
		source_ = mgr_.get(id);
		options_ = options;
	}

	string opCall() {
		return parse();
	}

private:
	string parse() {
		string errors;

		try {
			++envs_.length;
			++defs_.length;

			return parse(source_, SourceLoc(source_.id, 1, 0));
		} catch(Exception error) {
			if (auto ctxError = cast(ContextException)error) {
				errors = format("%s: %s", mgr_.loc(ctxError.loc), error.msg);
			} else if (auto preError = cast(PreParserException)error) {
				errors = format("%s: %s", mgr_.loc(preError.loc), error.msg);
			} else {
				errors = format("%s: %s", mgr_.loc(contexts_.back.loc), error.msg);
			}

			if (!contexts_.empty) {
				foreach_reverse (context; contexts_[0..$-1])
					errors ~= format("\n> %s", mgr_.loc(context.loc));
			}
		}

		throw new Exception(errors);
	}

	string parse(Source source, SourceLoc loc) {
		auto context = new Context(source, loc);
		contexts_ ~= context;
		scope (success) --contexts_.length;

		Appender!string app;
		app.reserve(16 * 1024);

		const end = context.remaining.length - 2;
		while (context.cursor < end) {
			auto remaining = context.remaining;
			auto indexOpen = remaining.indexOf("{{");
			while (indexOpen != -1) {
				auto tag = remaining[indexOpen + 2..indexOpen + 2 + 1];
				if ((tag == "#") || (tag == "&") || (tag == "!"))
					break;
				indexOpen = remaining.indexOf("{{", indexOpen + 2);
			}
			if (indexOpen == -1)
				break;

			if (!def_)
				app.put(remaining[0..indexOpen]);
			context.advance(indexOpen);

			const contentStart = indexOpen + 2;
			auto indexClose = remaining.indexOf("}}", contentStart);
			if (indexClose == -1)
				throw new PreParserException(context.loc, "missing '}}' to close tag '{{'");

			context.advance(2);
			indexClose -= contentStart;

			auto replaced = replacer(context.source.buffer[context.cursor..context.cursor + indexClose], context);
			if (!def_)
				app.put(replaced);
			context.advance(indexClose + 2);
		}
		context.expectClosed();

		app.put((context.cursor > 0) ? context.remaining() : context.source.buffer);

		if (needsLineNumbers(app.data))
			return sourceInfo(loc) ~ app.data;
		return app.data;
	}

	string include(string content, Context context) {
		auto embed = ((content.length > 1) && (content[1] == content[0])) ? 1 : 0;
		auto source = mgr_.open(content[1 + embed..$].strip, embed != 0, context.source.id);
		mgr_.dependency(context.source.id, source.id, context.loc);

		string result;
		if (!embed) {
			result = parse(source, SourceLoc(source.id, 1, 0));
		} else {
			auto name = mgr_.name(source.id);
			result = format("data:%s;base64,%s", name.mimeType, encode(source.buffer));
		}

		if (needsLineNumbers(result))
			result ~= sourceInfo(context.loc);
		return result;
	}

	string define(string content, Context context) {
		auto lex = Lexer(Source(source_.id, source_.parent, content[1..$].strip));
		auto tok = expect(lex, context, Token.KeywordKind.Def, Token.KeywordKind.Undef, Token.KeywordKind.Push, Token.KeywordKind.Pop, Token.KeywordKind.Set, Token.Kind.Identifier, "/");

		if (tok.keyword(Token.KeywordKind.Def)) {
			auto def = parseDef(lex, context);

			if (auto pdef = def.name in defs_.back) {
				if (pdef.loc != def.loc)
					throw new PreParserException(context.loc, format("redefinition of macro '%s' - first defined in '%s' - if this is intended undefine first", def.name, mgr_.loc(pdef.loc)));
			}

			if (def.flags & Def.Flags.Inline) {
				defs_.back[def.name] = def;
			} else {
				context.open(content[0..1], content[1..$]);
				def.flags |= Def.Flags.NotYetDefined;

				if (depth_ == 0)
					def_ = &(defs_.back[def.name] = def);
				++depth_;
			}
		} else if (tok.keyword(Token.KeywordKind.Undef)) {
			tok = expect(lex, context, Token.Kind.Identifier);

			auto name = tok.value;
			if (auto pdef = name in defs_.back) {
				if (pdef.flags & Def.Flags.NotYetDefined)
					throw new PreParserException(context.loc, format("trying to undefine macro '%s' inside it's own definition", name));

				defs_.back.remove(name);
			} else {
				throw new PreParserException(context.loc, format("trying to undefine unknown macro '%s'", name));
			}
		} else if (tok.keyword(Token.KeywordKind.Set) || tok.keyword(Token.KeywordKind.Push)) {
			auto name = expect(lex, context, Token.Kind.Identifier);
			auto value = expect(lex, context, Token.Kind.Literal, Token.KeywordKind.True, Token.KeywordKind.False);
			expect(lex, context, Token.Kind.EndOfInput);

			return "{{" ~ content ~ "}}";
		} else if (tok.keyword(Token.KeywordKind.Pop)) {
			auto name = expect(lex, context, Token.Kind.Identifier);
			expect(lex, context, Token.Kind.EndOfInput);

			return "{{" ~ content ~ "}}";
		} else if (tok.ident) {
			if (!def_)
				return expand(lex, context, tok.value);
		} else {
			close(lex, context);
		}

		return null;
	}

	void close(ref Lexer lex, Context context) {
		expect(lex, context, Token.Kind.EndOfInput);

		auto tag = context.close();

		if (depth_ == 1) {
			auto start = tag.cursor + tag.content.length + 3;
			auto end = context.cursor - 2;
			auto value = context.source.buffer[start..end];

			assert(def_);
			assert(def_.flags & Def.Flags.NotYetDefined);

			def_.value = value;
			def_.flags &= ~Def.Flags.NotYetDefined;
			def_ = null;
		}

		--depth_;
	}

	string expand(ref Lexer lex, Context context, string name) {
		auto optional = (lex.front.kind == Token.Kind.Separator) && (lex.front.name == "?");
		if (optional)
			lex.popFront;

		foreach (k; 0..envs_.length) {
			auto top = envs_.length - k - 1;
			auto env = envs_[top];

			if (auto penv = name in env) {
				if (needsLineNumbers(*penv))
					return *penv ~ sourceInfo(context.loc);
				return *penv;
			}

			auto defs = defs_[top];
			if (auto pdef = name in defs) {
				auto tok = lex.front;

				string[] args;

				if (!tok.eoi && tok.sep('(')) {
					lex.popFront;

					tok = expect(lex, context, ")", Token.Kind.Identifier, Token.Kind.Literal);

					if (!tok.sep(')')) {
						while (true) {
							if (tok.ident) {
								args ~= expand(lex, context, tok.value);
							} else {
								args ~= tok.unescaped;
							}

							tok = expect(lex, context, ",", ")");
							if (tok.sep(',')) {
								tok = expect(lex, context, ")", Token.Kind.Identifier, Token.Kind.Literal);
								continue;
							}
							if (tok.sep(')'))
								break;
						}
					}

					if (pdef.args.length < args.length)
						throw new PreParserException(context.loc, format("too many parameters for macro '%s'", pdef.pretty(name)));
				}

				string[string] envArgs;

				foreach(i, arg; pdef.args)
					envArgs[pdef.args[i]] = (i < args.length) ? args[i] : null;

				{
					++defs_.length;
					scope(exit) --defs_.length;

					envs_ ~= envArgs;
					scope(exit) --envs_.length;

					auto source = mgr_.add(format("#%s at %s", pdef.name, mgr_.name(pdef.loc.id)), pdef.value, pdef.loc.id);
					auto result = parse(source, SourceLoc(source.id, pdef.loc.line, pdef.loc.column));
					if (needsLineNumbers(result))
						result ~= sourceInfo(context.loc);
					return result;
				}
			}
		}

		if (!optional)
			throw new PreParserException(context.loc, format("unknown macro '%s'", name));
		return null;
	}

	string[] parseFormalArgList(ref Lexer lex, Context context) {
		string[] args;

		auto tok = expect(lex, context, ")", Token.Kind.Identifier);

		if (!tok.sep(')')) {
			while (true) {
				args ~= tok.value;

				tok = expect(lex, context, ",", ")");
				if (tok.sep(',')) {
					tok = expect(lex, context, ")", Token.Kind.Identifier);
					continue;
				}
				if (tok.sep(')'))
					break;
			}
		}

		return args;
	}


	Def parseDef(ref Lexer lex, Context context) {
		auto tok = expect(lex, context, Token.Kind.Identifier);

		Def def;
		def.name = tok.value;
		def.loc = context.loc;

		tok = expect(lex, context, "(", ":", Token.Kind.EndOfInput);

		if (tok.kind != Token.Kind.EndOfInput) {
			if (tok.sep("("))
				def.args = parseFormalArgList(lex, context);

			if (tok.sep(":")) {
				def.flags |= Def.Flags.Inline;
				lex.popFront;
			} else {
				expect(lex, context, Token.Kind.EndOfInput);
			}
		}

		return def;
	}

	string replacer(string content, Context context) {
		if (content.length > 0) {
			auto tag = content[0];
			switch(tag) {
			case '!':
				return null;
			case '&':
				return include(content, context).strip;
			case '#':
				return define(content, context).strip;
			default:
				assert(0);
			}
		}
		return null;
	}

	auto needsLineNumbers(string content) const {
		return options_.lineNumbers && !isAllWhite(content);
	}

	string sourceInfo(SourceLoc loc) {
		return format("{{;src:%d:%d:%d %s}}", loc.id, loc.line, loc.column, mgr_.name(loc.id));
	}

private:
	Token expectNot(Args...)(ref Lexer lex, Context context, auto ref Args args) {
		auto tok = lex.front;
		lex.popFront;

		foreach(i, Arg; Args) {
			static assert(is(Arg == Token) || isSomeString!Arg || is(Arg == Token.Kind) || is(Arg == Token.KeywordKind));
			static if (is(Arg == Token)) {
				if (tok != args[i])
					continue;
			} else static if (isSomeString!Arg) {
				if ((tok.kind != Token.Kind.Separator) || (tok.value != args[i]))
					continue;
			} else static if (is(Arg == Token.Kind)) {
				if (tok.kind != args[i])
					continue;
			} else static if (is(Arg == Token.KeywordKind)) {
				if (!tok.keyword(args[i]))
					continue;
			}

			{
				static if (is(Arg == Token)) {
					throw new PreParserException(context.loc, format("unexpected '%s'", args[i]));
				} else {
					throw new PreParserException(context.loc, format("unexpected '%s'", tok.value));
				}
			}
		}

		return tok;
	}

	Token expect(Args...)(ref Lexer lex, Context context, auto ref Args args) {
		auto tok = lex.front;
		lex.popFront;

		foreach(i, Arg; Args) {
			static assert(is(Arg == Token) || isSomeString!Arg || is(Arg == Token.Kind) || is(Arg == Token.KeywordKind));
			static if (is(Arg == Token)) {
				if (tok == args[i])
					return tok;
			} else static if (isSomeString!Arg) {
				if ((tok.kind == Token.Kind.Separator) && (tok.value == args[i]))
					return tok;
			} else static if (is(Arg == Token.Kind)) {
				if (tok.kind == args[i])
					return tok;
			} else static if (is(Arg == Token.KeywordKind)) {
				if (tok.keyword(args[i]))
					return tok;
			}
		}

		auto exception = appender!string;

		foreach(i, arg; args) {
			if (i == 0) {
				formattedWrite(&exception, "expected '%s'", arg);
			} else if (i + 1 == args.length) {
				formattedWrite(&exception, ", or '%s'", arg);
			} else {
				formattedWrite(&exception, ", '%s'", arg);
			}
		}

		if (tok.kind == Token.Kind.Separator) {
			formattedWrite(&exception, " but found '%s'", tok);
		} else {
			formattedWrite(&exception, " but found '%s'", tok);
		}

		throw new PreParserException(context.loc, exception.data);
	}

	static struct Def {
		enum Flags : uint {
			NotYetDefined   = 1 << 0,
			Inline          = 1 << 1,
		}

		string name;
		string value;
		string[] args;
		uint flags;

		SourceLoc loc;

		string pretty(string name) const {
			Appender!string app;
			app.reserve(1024);

			app.put(name);

			app.put("(");

			foreach(i, arg; args) {
				app.put(arg);
				if (i != args.length - 1)
					app.put(", ");
			}
			app.put(")");

			return app.data;
		}
	}

	Def* def_;
	size_t depth_;

	Def[string][] defs_;
	string[string][] envs_;

	Source source_;

	PreParserOptions options_;

	SourceManager* mgr_;
	Context[] contexts_;
}


private @property bool isAllWhite(R)(R range) {
	import std.uni : isWhite;
	foreach (ch; range) {
		if (!isWhite(ch))
			return false;
	}
	return true;
}
