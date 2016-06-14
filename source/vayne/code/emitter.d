module vayne.code.emitter;


import std.algorithm;
import std.array;
import std.string;

import vayne.ast.node;
import vayne.op;
import vayne.source.source;
import vayne.source.token;


private alias Value = Operand;


struct Emitter {
	auto instrs() const {
		return instrs_;
	}

	auto locs() const {
		return locs_;
	}

	auto registerCount() const {
		return registers_;
	}

	auto constantCount() const {
		return cast(uint)constants_.length;
	}

	static struct ConstantSlot {
		enum Type : ubyte {
			Global = 0,
			Boolean,
			String,
			Integer,
			Float,
		}

		Type type;
		string value;
	}

	auto constants() const {
		return constants_;
	}

	void emitModule(Module node) {
		assert(scopes_.empty);
		assert(registers_ == 0);

		foreach (child; node.children)
			emitStatement(child);

		assert(scopes_.empty);
		assert(registers_ == frees_.length);
		assert(!registers_ || refs_.reduce!((a, b) => a + b) == 0);
	}

private:
	Value emitExpression(Node node) {
		assert(node !is null);
		if (auto expr = cast(Expression)node) {
			return emitExpression(expr.children[0]);
		} else if (auto konst = cast(Constant)node) {
			return emitConstant(konst);
		} else if (auto ident = cast(Identifier)node) {
			return emitIdentifier(ident);
		} else if (auto binop = cast(BinaryOp)node) {
			return emitBinaryOp(binop);
		} else if (auto uniop = cast(UnaryOp)node) {
			return emitUnaryOp(uniop);
		} else if (auto condexpr = cast(ConditionalExpression)node) {
			return emitConditionalExpression(condexpr);
		} else if (auto indexop = cast(IndexOp)node) {
			return emitIndexOp(indexop);
		} else if (auto dispatchop = cast(DispatchOp)node) {
			return emitDispatchOp(dispatchop);
		} else if (auto call = cast(FunctionCall)node) {
			return emitFunctionCall(call);
		} else {
			assert(false, "unimplemented expression " ~ typeid(node).toString);
		}
	}

	auto emitConstant(Constant node) {
		assert((node.tok.kind == Token.Kind.Literal) || (node.tok.kind == Token.Kind.Keyword));

		if (node.tok.kind == Token.Kind.Literal) {
			final switch (node.tok.kindLiteral) with (Token.LiteralKind) {
			case Char:
			case String:
				return constant(ConstantSlot.Type.String, node.tok.value);
			case Bin:
				return constant(ConstantSlot.Type.Integer, node.tok.value);
			case Oct:
				return constant(ConstantSlot.Type.Integer, node.tok.value);
			case Dec:
				return constant(ConstantSlot.Type.Integer, node.tok.value);
			case Hex:
				return constant(ConstantSlot.Type.Integer, node.tok.value);
			case Float:
				return constant(ConstantSlot.Type.Float, node.tok.value);
			}
		} else {
			final switch (node.tok.kindKeyword) with (Token.KeywordKind) {
			case True:
				return constant(ConstantSlot.Type.Boolean, "true");
			case False:
				return constant(ConstantSlot.Type.Boolean, "false");
			case Null:
			case In:
			case Def:
			case Undef:
				assert(false, "unimplemented keyword kind constant " ~ node.tok.kindKeyword);
			}
		}
	}

	auto emitIdentifier(Identifier node) {
		return aquire(findSymbol(node.tok.value));
	}

	auto emitUnaryOp(UnaryOp node) {
		auto expr = emitExpression(node.children[0]);
		switch (node.tok.name) {
		case "-":
			auto target = registerize(node.tok.loc, expr);
			emit!(OpCode.Minus)(node.tok.loc, target);
			return target;
		case "!":
			auto target = registerize(node.tok.loc, expr);
			emit!(OpCode.Not)(node.tok.loc, target);
			return target;
		case "+":
			return expr;
		default:
			assert(false, "unimplemented unary op " ~ node.tok.toString);
		}
	}

	auto emitBinaryOp(BinaryOp node) {
		auto lhs = emitExpression(node.children[0]);
		auto rhs = emitExpression(node.children[1]);
		scope (exit) release(lhs, rhs);

		auto target = register();

		switch (node.tok.name) {
		case "&&":
			emit!(OpCode.And)(node.tok.loc, target, lhs, rhs);
			break;
		case "||":
			emit!(OpCode.Or)(node.tok.loc, target, lhs, rhs);
			break;
		case "==":
			emit!(OpCode.Equal)(node.tok.loc, target, lhs, rhs);
			break;
		case "!=":
			emit!(OpCode.NotEqual)(node.tok.loc, target, lhs, rhs);
			break;
		case ">":
			emit!(OpCode.Greater)(node.tok.loc, target, lhs, rhs);
			break;
		case ">=":
			emit!(OpCode.GreaterOrEqual)(node.tok.loc, target, lhs, rhs);
			break;
		case "<":
			emit!(OpCode.Less)(node.tok.loc, target, lhs, rhs);
			break;
		case "<=":
			emit!(OpCode.LessOrEqual)(node.tok.loc, target, lhs, rhs);
			break;
		case "+":
			emit!(OpCode.Add)(node.tok.loc, target, lhs, rhs);
			break;
		case "-":
			emit!(OpCode.Subtract)(node.tok.loc, target, lhs, rhs);
			break;
		case "*":
			emit!(OpCode.Multiply)(node.tok.loc, target, lhs, rhs);
			break;
		case "/":
			emit!(OpCode.Divide)(node.tok.loc, target, lhs, rhs);
			break;
		case "%":
			emit!(OpCode.Remainder)(node.tok.loc, target, lhs, rhs);
			break;
		case "~":
			emit!(OpCode.Concat)(node.tok.loc, target, lhs, rhs);
			break;
		case "^^":
			emit!(OpCode.Power)(node.tok.loc, target, lhs, rhs);
			break;
		case "in":
			emit!(OpCode.TestKey)(node.tok.loc, target, rhs, lhs);
			break;
		default:
			assert(false, "unimplemented binary op " ~ node.tok.toString);
		}

		return target;
	}

	auto emitConditionalExpression(ConditionalExpression node) {
		auto cond = registerize(node.tok.loc, emitExpression(node.children[0]));

		emit!(OpCode.Test)(node.tok.loc, cond, cond);
		auto jz = placeholder(node.tok.loc);

		emit!(OpCode.Move)(node.tok.loc, cond, emitExpression(node.children[1]));

		auto jmp = placeholder(node.tok.loc);
		auto ipfalse = ip;

		emit!(OpCode.Move)(node.tok.loc, cond, emitExpression(node.children[2]));

		emitAt!(OpCode.JumpIfZero)(node.tok.loc, jz, Value(Value.Kind.Immediate, ipfalse), cond);
		emitAt!(OpCode.Jump)(node.tok.loc, jmp, Value(Value.Kind.Immediate, ip));

		return cond;
	}

	auto emitIndexOp(IndexOp node) {
		auto target = registerize(node.tok.loc, emitExpression(node.children[0]));
		auto key = emitExpression(node.children[1]);

		emit!(OpCode.Element)(node.tok.loc, target, target, key);
		return target;
	}

	auto emitDispatchOp(DispatchOp node) {
		auto target = registerize(node.tok.loc, emitExpression(node.children[0]));
		auto key = constant(ConstantSlot.Type.String, node.target.value);

		emit!(OpCode.Element)(node.tok.loc, target, target, key);
		return target;
	}

	auto emitFunctionCall(FunctionCall node) {
		auto result = register();
		auto func = emitExpression(node.children[0]);
		auto args = node.children[1..$];

		if (args.length) {
			auto base = registers(args.length);

			foreach (i, arg; args) {
				auto argValue = emitExpression(arg);
				emit!(OpCode.Move)(node.tok.loc, Value(Value.Kind.Register, base.value + cast(uint)i), argValue);
				release(argValue);
			}

			emit!(OpCode.Call)(node.tok.loc, result, func, base, Value(Value.Kind.Immediate, cast(uint)args.length));

			foreach (i; 0..args.length)
				release(Value(Value.Kind.Register, base.value + cast(uint)(args.length - i - 1)));
		} else {
			emit!(OpCode.Call)(node.tok.loc, result, func, Value(Value.Kind.Register, 0), Value(Value.Kind.Immediate, 0));
		}
		return result;
	}

	void emitOutput(Output node) {
		auto expr = emitExpression(node.children[0]);
		emit!(OpCode.Output)(node.tok.loc, expr);
		release(expr);
	}

	void emitStatement(Node node) {
		if (auto output = cast(Output)node) {
			emitOutput(output);
		} else if (auto ifstmt = cast(IfStatement)node) {
			emitIfStatement(ifstmt);
		} else if (auto loopstmt = cast(LoopStatement)node) {
			emitLoopStatement(loopstmt);
		} else if (auto stmtblock = cast(StatementBlock)node) {
			emitStatementBlock(stmtblock);
		} else {
			assert(false, "unimplemented statement " ~ node.tok.toString);
		}
	}

	void emitStatementBlock(StatementBlock node) {
		foreach (child; node.children)
			emitStatement(child);
	}

	void emitIfStatement(IfStatement node) {
		auto expr = emitExpression(node.children[0]);
		auto test = (instrs_.empty || !instrs_.back.isBoolean);
		auto cond = test ? register() : expr;

		if (test) {
			emit!(OpCode.Test)(node.tok.loc, cond, expr);
			release(expr);
		}

		size_t jumpTrueCase = placeholder(node.tok.loc);
		release(cond);

		emitStatement(node.children[1]);

		size_t jumpFalseCase;
		if (node.children[2] !is null)
			jumpFalseCase = placeholder(node.tok.loc);

		emitAt!(OpCode.JumpIfZero)(node.tok.loc, jumpTrueCase, Value(Value.Kind.Immediate, ip), cond);
		if (node.children[2] !is null) {
			emitStatement(node.children[2]);
			emitAt!(OpCode.Jump)(node.tok.loc, jumpFalseCase, Value(Value.Kind.Immediate, ip));
		}
	}

	void emitLoopStatement(LoopStatement node) {
		auto body_ = cast(StatementBlock)node.children[2];
		assert(body_ !is null);

		pushScope();

		if (node.children[1]) {
			// numeric range
			auto end = emitExpression(node.children[1]);
			auto it = registerize(node.tok.loc, emitExpression(node.children[0]));
			auto value = it;

			if (!node.key.empty)
				addSymbol(node.key.value, aquire(it));
			if (!node.value.empty)
				addSymbol(node.value.value, aquire(value));

			auto test = ip;
			auto cond = register();
			emit!(OpCode.Less)(node.tok.loc, cond, it, end);
			size_t jumpBody = placeholder(node.tok.loc);
			release(cond);

			emitStatementBlock(body_);
			emit!(OpCode.Increment)(node.tok.loc, it);
			emit!(OpCode.Jump)(node.tok.loc, Value(Value.Kind.Immediate, test));
			emitAt!(OpCode.JumpIfZero)(node.tok.loc, jumpBody, Value(Value.Kind.Immediate, ip), cond);
			release(it, end);
		} else {
			// array / object
			auto obj = emitExpression(node.children[0]);
			auto len = register();

			emit!(OpCode.Length)(node.tok.loc, len, obj);

			auto it = registerize(node.tok.loc, constant(ConstantSlot.Type.Integer, "0"));
			auto key = register();
			auto value = register();

			if (!node.key.empty)
				addSymbol(node.key.value, aquire(key));
			if (!node.value.empty)
				addSymbol(node.value.value, aquire(value));

			auto test = ip;
			auto cond = register();
			emit!(OpCode.Less)(node.tok.loc, cond, it, len);
			size_t jumpBody = placeholder(node.tok.loc);
			release(cond);

			emit!(OpCode.Key)(node.tok.loc, key, obj, it);
			emit!(OpCode.Element)(node.tok.loc, value, obj, key);

			emitStatementBlock(body_);
			emit!(OpCode.Increment)(node.tok.loc, it);
			emit!(OpCode.Jump)(node.tok.loc, Value(Value.Kind.Immediate, test));
			emitAt!(OpCode.JumpIfZero)(node.tok.loc, jumpBody, Value(Value.Kind.Immediate, ip), cond);
			release(it, len, obj, key, value);
		}

		popScope();
	}

	Value register() {
		if (frees_.empty) {
			refs_ ~= cast(size_t)1;
			return Value(Value.Kind.Register, registers_++);
		}
		auto free = frees_.back;
		frees_.popBack;

		assert(free < registers_);
		assert(refs_[free] == 0);
		++refs_[free];
		return Value(Value.Kind.Register, free);
	}

	auto registers(size_t count) {
		if (count == 1) {
			return register();
		} else {
			if (frees_.length >= count) {
				// TODO: be smarter
			}

			auto index = registers_;
			registers_ += count;
			foreach (i; 0..count)
				refs_ ~= cast(size_t)1;

			return Value(Value.Kind.Register, index);
		}
	}

	auto aquire(Value v) {
		if (v.kind == Value.Kind.Register)
			++refs_[v.value];
		return v;
	}

	void release(Values...)(Values values) {
		foreach (v; values) {
			if (v.kind == Value.Kind.Register) {
				if (refs_[v.value] > 0) {
					--refs_[v.value];
					if (refs_[v.value] == 0)
						frees_ ~= v.value;
				} else {
					assert(!frees_.canFind(v.value), format("double releasing register %s", v.value));
				}
			}
		}
	}

	auto constant(ConstantSlot.Type type, string value) {
		auto map = indexConstants_[type];
		if (auto pvalue = value in map)
			return Value(Value.Kind.Constant, *pvalue);

		auto slot = cast(uint)constants_.length;
		indexConstants_[type][value] = slot;
		constants_ ~= ConstantSlot(type, value);

		return Value(Value.Kind.Constant, slot);
	}

	auto registerize(SourceLoc loc, Value value) {
		if (value.kind != Value.Kind.Register) {
			assert(value.kind == Value.Kind.Constant);

			auto target = register();
			emit!(OpCode.Move)(loc, target, value);
			return target;
		}
		return value;
	}

	auto placeholder(SourceLoc loc) {
		instrs_ ~= encode!(OpCode.Nop)();
		locs_ ~= loc;
		return cast(uint)instrs_.length - 1;
	}

	auto ip() const {
		return cast(uint)instrs_.length;
	}

	auto emit(OpCode op, Args...)(SourceLoc loc, Args args) {
		instrs_ ~= encode!op(args);
		locs_ ~= loc;
		return instrs_.length - 1;
	}

	auto emitAt(OpCode op, Args...)(SourceLoc loc, size_t at, Args args) {
		instrs_[at] = encode!op(args);
		locs_[at] = loc;
	}

	void pushScope() {
		++scopes_.length;
	}

	void addSymbol(string name, Value value) {
		assert(!name.empty);

		foreach_reverse(s; scopes_) {
			if (auto pvalue = name in s)
				throw new Exception(format("symbol %s shadows symbol with same name %s", name, name));
		}
		scopes_.back[name] = value;
	}

	Value findSymbol(string name) {
		foreach_reverse (s; scopes_) {
			if (auto pvalue = name in s)
				return *pvalue;
		}

		return constant(ConstantSlot.Type.Global, name);
	}

	void popScope() {
		foreach (k, v; scopes_.back)
			release(v);
		--scopes_.length;
	}

	uint[] frees_;
	uint[] refs_;
	uint registers_;

	Value[string][] scopes_;

	ConstantSlot[] constants_;
	uint[string][5] indexConstants_;

	Instr[] instrs_;
	SourceLoc[] locs_;
}
