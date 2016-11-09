module vayne.code.emitter;


import std.algorithm;
import std.array;
import std.string;

import vayne.ast.node;
import vayne.ast.printer;
import vayne.op;
import vayne.source.source;
import vayne.source.token;


private alias Value = Operand;


enum Guard = q{
	auto __refs = reduce!((a, b) => a + b)(0, refs_);
	scope (success) {
		auto refs = reduce!((a, b) => a + b)(0, refs_);
		assert((refs == __refs) || (refs == 1 + __refs), "leaking registers!");
	}
	auto __scopes = scopes_.length;
	scope (success) {
		auto scopes = scopes_.length;
		assert(scopes == __scopes, "leaking scopes!");
	}
};


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
			Null,
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
		assert(!registers_ || reduce!((a, b) => a + b)(0, refs_) == 0);
		assert(registers_ == frees_.length);
	}

private:
	Value emitExpression(Node node) {
		debug mixin(Guard);

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
		} else if (auto preop = cast(PrefixOp)node) {
			return emitPrefixOp(preop);
		} else if (auto sufop = cast(SuffixOp)node) {
			return emitSuffixOp(sufop);
		} else if (auto condexpr = cast(ConditionalExpression)node) {
			return emitConditionalExpression(condexpr);
		} else if (auto indexop = cast(IndexOp)node) {
			return emitIndexOp(indexop);
		} else if (auto sliceop = cast(SliceOp)node) {
			return emitSliceOp(sliceop);
		} else if (auto dispatchop = cast(DispatchOp)node) {
			return emitDispatchOp(dispatchop);
		} else if (auto call = cast(FunctionCall)node) {
			return emitFunctionCall(call);
		} else {
			assert(false, "unimplemented expression " ~ typeid(node).toString);
		}
	}

	auto emitConstant(Constant node) {
		debug mixin(Guard);

		assert((node.tok.kind == Token.Kind.Literal) || (node.tok.kind == Token.Kind.Keyword));

		if (node.tok.kind == Token.Kind.Literal) {
			final switch (node.tok.kindLiteral) with (Token.LiteralKind) {
			case Char:
			case String:
				return constant(ConstantSlot.Type.String, node.tok.unescaped);
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
				return constant(ConstantSlot.Type.Null, "null");
			case In:
			case Set:
			case Def:
			case Undef:
			case Push:
			case Pop:
			case As:
				assert(false, "unimplemented keyword kind constant " ~ node.tok.kindKeyword);
			}
		}
	}

	auto emitIdentifier(Identifier node) {
		debug mixin(Guard);

		return findSymbol(node.tok.value, node.tok.loc);
	}

	auto emitUnaryOp(UnaryOp node) {
		debug mixin(Guard);

		auto expr = emitExpression(node.children[0]);
		scope(exit) release(expr);

		switch (node.tok.name) {
		case "-":
			auto target = register();
			emit(OpCode.Move, node.tok.loc, target, expr);
			emit(OpCode.Minus, node.tok.loc, target);
			return target;
		case "!":
			auto target = register();
			emit(OpCode.Move, node.tok.loc, target, expr);
			emit(OpCode.Not, node.tok.loc, target);
			return target;
		case "+":
			return expr;
		default:
			assert(false, "unimplemented unary op " ~ node.tok.toString);
		}
	}

	auto emitPrefixOp(PrefixOp node) {
		debug mixin(Guard);

		auto expr = registerize(node.tok.loc, emitExpression(node.children[0]));
		scope(exit) release(expr);

		switch (node.tok.name) {
		case "++":
			auto target = register();
			emit(OpCode.Increment, node.tok.loc, expr);
			emit(OpCode.Move, node.tok.loc, target, expr);
			return target;
		case "--":
			auto target = register();
			emit(OpCode.Decrement, node.tok.loc, expr);
			emit(OpCode.Move, node.tok.loc, target, expr);
			return target;
		default:
			assert(false, "unimplemented prefix op " ~ node.tok.toString);
		}
	}

	auto emitSuffixOp(SuffixOp node) {
		debug mixin(Guard);

		auto expr = registerize(node.tok.loc, emitExpression(node.children[0]));
		scope(exit) release(expr);

		switch (node.tok.name) {
		case "++":
			auto target = register();
			emit(OpCode.Move, node.tok.loc, target, expr);
			emit(OpCode.Increment, node.tok.loc, expr);
			return target;
		case "--":
			auto target = register();
			emit(OpCode.Move, node.tok.loc, target, expr);
			emit(OpCode.Decrement, node.tok.loc, expr);
			return target;
		default:
			assert(false, "unimplemented suffix op " ~ node.tok.toString);
		}
	}

	auto emitBinaryOp(BinaryOp node) {
		debug mixin(Guard);

		{
			switch (node.tok.name) {
			case "&&":
			case "||":
				auto target = register();
				auto lhs = emitExpression(node.children[0]);
				emit(OpCode.Test, node.tok.loc, target, lhs);
				release(lhs);

				size_t jumpShortCircuit = placeholder(node.tok.loc);

				auto rhs = emitExpression(node.children[1]);
				emit(OpCode.Test, node.tok.loc, target, rhs);
				release(rhs);

				emitAt((node.tok.name == "&&") ? OpCode.JumpIfZero : OpCode.JumpIfNotZero, node.tok.loc, jumpShortCircuit, Value(Value.Kind.Immediate, ip), target);
				return target;
			default:
				break;
			}
		}

		{
			auto lhs = emitExpression(node.children[0]);
			auto rhs = emitExpression(node.children[1]);
			scope (exit) release(lhs, rhs);

			auto target = register();

			switch (node.tok.name) {
			case "==":
				emit(OpCode.Equal, node.tok.loc, target, lhs, rhs);
				break;
			case "!=":
				emit(OpCode.NotEqual, node.tok.loc, target, lhs, rhs);
				break;
			case ">":
				emit(OpCode.Greater, node.tok.loc, target, lhs, rhs);
				break;
			case ">=":
				emit(OpCode.GreaterOrEqual, node.tok.loc, target, lhs, rhs);
				break;
			case "<":
				emit(OpCode.Less, node.tok.loc, target, lhs, rhs);
				break;
			case "<=":
				emit(OpCode.LessOrEqual, node.tok.loc, target, lhs, rhs);
				break;
			case "+":
				emit(OpCode.Add, node.tok.loc, target, lhs, rhs);
				break;
			case "-":
				emit(OpCode.Subtract, node.tok.loc, target, lhs, rhs);
				break;
			case "*":
				emit(OpCode.Multiply, node.tok.loc, target, lhs, rhs);
				break;
			case "/":
				emit(OpCode.Divide, node.tok.loc, target, lhs, rhs);
				break;
			case "%":
				emit(OpCode.Remainder, node.tok.loc, target, lhs, rhs);
				break;
			case "~":
				emit(OpCode.Concat, node.tok.loc, target, lhs, rhs);
				break;
			case "^^":
				emit(OpCode.Power, node.tok.loc, target, lhs, rhs);
				break;
			case "in":
				emit(OpCode.TestKey, node.tok.loc, target, rhs, lhs);
				break;
			default:
				assert(false, "unimplemented binary op " ~ node.tok.toString);
			}

			return target;
		}
	}

	auto emitConditionalExpression(ConditionalExpression node) {
		debug mixin(Guard);

		auto target = register();

		auto cond = emitExpression(node.children[0]);
		scope(exit) release(cond);

		emit(OpCode.Test, node.tok.loc, target, cond);
		auto jz = placeholder(node.tok.loc);

		auto trueCase = emitExpression(node.children[1]);
		emit(OpCode.Move, node.tok.loc, target, trueCase);
		release(trueCase);

		auto jmp = placeholder(node.tok.loc);
		auto ipfalse = ip;

		auto falseCase = emitExpression(node.children[2]);
		emit(OpCode.Move, node.tok.loc, target, falseCase);
		release(falseCase);

		emitAt(OpCode.JumpIfZero, node.tok.loc, jz, Value(Value.Kind.Immediate, ipfalse), target);
		emitAt(OpCode.Jump, node.tok.loc, jmp, Value(Value.Kind.Immediate, ip));

		assert(!canFind(frees_, target.value));
		assert(refs_[target.value] > 0);

		return target;
	}

	auto emitIndexOp(IndexOp node) {
		debug mixin(Guard);

		auto target = registerize(node.tok.loc, emitExpression(node.children[0]));
		auto index = emitExpression(node.children[1]);
		scope(exit) release(index);

		emit(OpCode.Element, node.tok.loc, target, target, index);
		return target;
	}

	auto emitSliceOp(SliceOp node) {
		debug mixin(Guard);

		auto target = registerize(node.tok.loc, emitExpression(node.children[0]));
		auto start = emitExpression(node.children[1]);
		auto end = emitExpression(node.children[2]);
		scope (exit) release(start, end);

		emit(OpCode.Slice, node.tok.loc, target, target, start, end);
		return target;
	}

	auto emitDispatchOp(DispatchOp node) {
		debug mixin(Guard);

		auto obj = emitExpression(node.children[0]);
		scope (exit) release(obj);

		auto key = constant(ConstantSlot.Type.String, node.target.value);

		auto target = register();
		emit(OpCode.Element, node.tok.loc, target, obj, key);
		return target;
	}

	auto emitDispatchOpForCall(DispatchOp node, Value obj) {
		auto expr = emitExpression(node.children[0]);
		scope (exit) release(expr);

		auto key = constant(ConstantSlot.Type.String, node.target.value);

		emit(OpCode.Move, node.tok.loc, obj, expr);

		auto target = register();
		emit(OpCode.Dispatch, node.tok.loc, target, expr, key);
		return target;
	}

	auto emitFunctionCall(FunctionCall node) {
		debug mixin(Guard);

		auto result = register();
		auto dispatched = (cast(DispatchOp)node.children[0] !is null);
		auto args = node.children[1..$];

		if (!dispatched) {
			auto func = emitExpression(node.children[0]);
			scope (exit) release(func);

			if (args.length) {
				auto base = registers(args.length);

				foreach (i, arg; args) {
					auto argValue = emitExpression(arg);
					emit(OpCode.Move, node.tok.loc, Value(Value.Kind.Register, base.value + cast(uint)i), argValue);
					release(argValue);
				}

				emit(OpCode.Call, node.tok.loc, result, func, base, Value(Value.Kind.Immediate, cast(uint)args.length));

				foreach (i; 0..args.length)
					release(Value(Value.Kind.Register, base.value + cast(uint)i));
			} else {
				emit(OpCode.Call, node.tok.loc, result, func, Value(Value.Kind.Register, 0), Value(Value.Kind.Immediate, 0));
			}
		} else {
			auto base = registers(1 + args.length);

			foreach (i, arg; args) {
				auto argValue = emitExpression(arg);
				emit(OpCode.Move, node.tok.loc, Value(Value.Kind.Register, base.value + 1 + cast(uint)i), argValue);
				release(argValue);
			}

			auto func = emitDispatchOpForCall(cast(DispatchOp)node.children[0], base);
			scope(exit) release(func);
			emit(OpCode.DispatchCall, node.tok.loc, result, func, base, Value(Value.Kind.Immediate, 1 + cast(uint)args.length));

			foreach (i; 0..args.length + 1)
				release(Value(Value.Kind.Register, base.value + cast(uint)i));
		}
		return result;
	}

	void emitOutput(Output node) {
		debug mixin(Guard);

		auto expr = emitExpression(node.children[0]);
		emit(OpCode.Output, node.tok.loc, expr);
		release(expr);
	}

	void emitStatement(Node node) {
		debug mixin(Guard);

		if (auto output = cast(Output)node) {
			emitOutput(output);
		} else if (auto ifstmt = cast(IfStatement)node) {
			emitIfStatement(ifstmt);
		} else if (auto loopstmt = cast(LoopStatement)node) {
			emitLoopStatement(loopstmt);
		} else if (auto call = cast(FunctionCall)node) {
			release(emitFunctionCall(call));
		} else if (auto withstmt = cast(WithStatement)node) {
			emitWithStatement(withstmt);
		} else if (auto stmtblock = cast(StatementBlock)node) {
			emitStatementBlock(stmtblock);
		} else {
			assert(false, "unimplemented statement " ~ node.tok.toString);
		}
	}

	void emitStatementBlock(StatementBlock node) {
		debug mixin(Guard);

		foreach (child; node.children)
			emitStatement(child);
	}

	void emitWithStatement(WithStatement node) {
		debug mixin(Guard);

		Value[] exprs;
		exprs.reserve(node.children.length - 1);

		pushScope();

		uint scopes;
		foreach (i, childNode; node.children[0..$-1]) {
			auto child = cast(WithExpression)childNode;
			assert(child !is null);

			auto expr = emitExpression(child.children[0]);

			if (child.name.empty) {
				++scopes;
				emit(OpCode.PushScope, child.tok.loc,  expr);
			} else {
				auto copy = register();
				scope(exit) release(copy);

				emit(OpCode.Move, child.tok.loc, copy, expr);
				swap(copy, expr);

				addSymbol(child.name.value, aquire(expr));
			}

			exprs ~= expr;
		}

		emitStatement(node.children[$ - 1]);
		if (scopes)
			emit(OpCode.PopScope, node.tok.loc, Value(Value.Kind.Immediate, scopes));

		foreach (expr; exprs)
			release(expr);

		popScope();
	}

	void emitIfStatement(IfStatement node) {
		debug mixin(Guard);

		auto expr = emitExpression(node.children[0]);
		auto cond = register();

		emit(OpCode.Test, node.tok.loc, cond, expr);
		release(expr);

		size_t jumpTrueCase = placeholder(node.tok.loc);
		release(cond);

		assert(canFind(frees_, cond.value));
		assert(refs_[cond.value] == 0);

		emitStatement(node.children[1]);

		size_t jumpFalseCase;
		if (node.children[2] !is null)
			jumpFalseCase = placeholder(node.tok.loc);

		emitAt(OpCode.JumpIfZero, node.tok.loc, jumpTrueCase, Value(Value.Kind.Immediate, ip), cond);
		if (node.children[2] !is null) {
			emitStatement(node.children[2]);
			emitAt(OpCode.Jump, node.tok.loc, jumpFalseCase, Value(Value.Kind.Immediate, ip));
		}
	}

	void emitLoopStatement(LoopStatement node) {
		debug mixin(Guard);

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
			emit(OpCode.Less, node.tok.loc, cond, it, end);
			size_t jumpBody = placeholder(node.tok.loc);
			release(cond);

			emitStatementBlock(body_);
			emit(OpCode.Increment, node.tok.loc, it);
			emit(OpCode.Jump, node.tok.loc, Value(Value.Kind.Immediate, test));
			emitAt(OpCode.JumpIfZero, node.tok.loc, jumpBody, Value(Value.Kind.Immediate, ip), cond);
			release(it, end);
		} else {
			// array / object
			auto obj = emitExpression(node.children[0]);
			auto keys = register();
			auto len = register();

			emit(OpCode.Keys, node.tok.loc, keys, obj);
			emit(OpCode.Length, node.tok.loc, len, keys);

			auto it = registerize(node.tok.loc, constant(ConstantSlot.Type.Integer, "0"));
			auto key = register();
			auto value = register();

			if (!node.key.empty)
				addSymbol(node.key.value, aquire(key));
			if (!node.value.empty)
				addSymbol(node.value.value, aquire(value));

			auto test = ip;
			auto cond = register();
			emit(OpCode.Less, node.tok.loc, cond, it, len);
			size_t jumpBody = placeholder(node.tok.loc);
			release(cond);

			emit(OpCode.Element, node.tok.loc, key, keys, it);
			emit(OpCode.Element, node.tok.loc, value, obj, key);

			emitStatementBlock(body_);
			emit(OpCode.Increment, node.tok.loc, it);
			emit(OpCode.Jump, node.tok.loc, Value(Value.Kind.Immediate, test));
			emitAt(OpCode.JumpIfZero, node.tok.loc, jumpBody, Value(Value.Kind.Immediate, ip), cond);
			release(it, keys, len, obj, key, value);
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
		} else if (count > 1) {
			auto index = registers_;

			if (frees_.length >= count) {
				frees_.sort();

				uint start = 0;
				while (start + count <= frees_.length) {
					if (frees_[start + count - 1] == frees_[start] + count - 1)
						break;
					++start;
				}

				if (start + count <= frees_.length) {
					index = frees_[start];

					frees_ = frees_[0..start] ~ frees_[start + count..$]; // TODO: find starting from the end, then manually copy + pop
				}
			}

			if (index == registers_) {
				registers_ += count;
				refs_.length = registers_;
			}

			foreach (i; index..index + count) {
				assert(refs_[i] == 0);
				refs_[i] = 1;
			}

			return Value(Value.Kind.Register, index);
		}

		return Value(Value.Kind.Register, uint.max);
	}

	auto aquire(Value v) {
		assert(!frees_.canFind(v.value), format("aquiring freed register %s", v.value));
		if (v.kind == Value.Kind.Register)
			++refs_[v.value];
		return v;
	}

	void release(Values...)(Values values) {
		foreach (v; values) {
			if (v.kind == Value.Kind.Register) {
				if (refs_[v.value] > 0) {
					--refs_[v.value];
					if (refs_[v.value] == 0) {
						assert(!frees_.canFind(v.value), format("double releasing register %s", v.value));
						frees_ ~= v.value;
					}
				} else {
					assert(false, format("double releasing register %s", v.value));
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
			emit(OpCode.Move, loc, target, value);
			return target;
		}
		return value;
	}

	auto placeholder(SourceLoc loc) {
		instrs_ ~= Instr(OpCode.Nop);
		locs_ ~= loc;
		return cast(uint)instrs_.length - 1;
	}

	auto ip() const {
		return cast(uint)instrs_.length;
	}

	auto emit(Args...)(OpCode op, SourceLoc loc, Args args) {
		instrs_ ~= Instr(op, args);
		locs_ ~= loc;
		return instrs_.length - 1;
	}

	auto emitAt(Args...)(OpCode op, SourceLoc loc, size_t at, Args args) {
		instrs_[at] = Instr(op, args);
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

	Value findSymbol(string name, SourceLoc loc) {
		foreach_reverse (s; scopes_) {
			if (auto pvalue = name in s)
				return aquire(*pvalue);
		}

		auto result = register();
		emit(OpCode.LookUp, loc, result, constant(ConstantSlot.Type.String, name));
		return result;
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
