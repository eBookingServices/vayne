module vayne.vm;


import std.conv;
import std.datetime;
import std.range;
import std.stdio;
import std.string;
import std.traits;


import vayne.op;
import vayne.value;
import vayne.source.source;

public import vayne.value : Value;


enum VMOptions : uint {
	PrintOutput		= 1 << 0,
	PrintOpCodes	= 1 << 1,
	DebugMode		= PrintOutput | PrintOpCodes,
	Default			= 0,
}


class VMException : Exception {
	this(string msg, string source, size_t line) {
		super(msg, source, line);
	}
}


struct VM(uint options = VMOptions.Default) {
	static struct Error {
		string msg;
		string source;
		size_t line;
	}
	alias ErrorHandler = void delegate(Error);
	alias Globals = Value[string];

	void load(size_t registers, Value[] constants, const(Instr)[] instrs, const(SourceLoc)[] locs, const(string)[] sources) {
		instrs_ = instrs;
		locs_ = locs;
		sources_ = sources;
		consts_ = constants;

		regs_.length = registers;
	}

	void load(size_t registers, size_t constants, const(Instr)[] instrs, const(SourceLoc)[] locs, const(string)[] sources) {
		instrs_ = instrs;
		locs_ = locs;
		sources_ = sources;

		regs_.length = registers;
		consts_.length = constants;
	}

	void bindConst(T)(size_t index, ref T value) if (is(T == struct)) {
		consts_[index] = Value(value);
	}

	void bindConst(T)(size_t index, in T value)  if (!is(T == struct)) {
		consts_[index] = Value(value);
	}

	void setGlobals(Globals globals) {
		globals_ = globals;
	}

	void bindGlobals(Globals globals) {
		foreach (k, v; globals)
			globals_[k] = v;
	}

	void bindGlobal(T)(string name, ref T value) if (is(T == struct)) {
		globals_[name] = Value(value);
	}

	void bindGlobal(T)(string name, in T value)  if (!is(T == struct)) {
		globals_[name] = Value(value);
	}

	@property void errorHandler(ErrorHandler handler) {
		errorHandler_ = handler;
	}

	@property ErrorHandler errorHandler() const {
		return errorHandler_;
	}

	void execute(T)(ref T output, Globals globals) if (isOutputRange!(T, char)) {
		globals_ = globals;

		execute(output);
	}

	void execute(T)(ref T output) if (isOutputRange!(T, char)) {
		Instr instr = instrs_[0];
		ulong ip;

		ref auto getArgV(size_t Arg)() if (Arg <= 3) {
			if (instr.argConst!Arg) {
				return consts_[instr.arg!Arg];
			} else {
				return regs_[instr.arg!Arg];
			}
		}

		ref auto argBinaryOp(size_t A, string op, size_t B)() {
			return getArgV!A.binaryOp!op(getArgV!B);
		}

		ref auto argCompareOp(size_t A, string op, size_t B)() {
			return Value(getArgV!A.compareOp!op(getArgV!B));
		}

		while (true) {
			const op = instr.op;

			static if (options & VMOptions.PrintOpCodes) {
				writeln(op);
			}

			try {
				Lswitch: final switch (op) with (OpCode) {
				case Nop:
					break;
				case Halt:
					return;
				case Move:
					regs_[instr.arg!0] = getArgV!1;
					break;
				case Throw:
					throw new Exception(getArgV!0.toString);
				case Output:
					output.put(getArgV!0.get!string);

					static if (options & VMOptions.PrintOutput) {
						write(getArgV!0.get!string);
					}
					break;
				case PushScope:
					auto s = getArgV!0;
					switch (s.type) with (Value.Type) {
					case Object:
					case AssocArray:
						scopes_ ~= getArgV!0;
						break;
					default:
						throw new Exception(format("with statement expressions must be of type %s or %s, not %s", Object, AssocArray, s.type));
					}
					break;
				case PopScope:
					scopes_.length = scopes_.length - instr.arg!0;
					break;
				case Minus:
					regs_[instr.arg!0].unaryOp!"-";
					break;
				case Not:
					regs_[instr.arg!0] = Value(!regs_[instr.arg!0].get!bool);
					break;
				case Decrement:
					regs_[instr.arg!0].unaryOp!"--";
					break;
				case Increment:
					regs_[instr.arg!0].unaryOp!"++";
					break;
				case Jump:
					ip = instr.arg!0;
					instr = instrs_[ip];
					continue;
				case JumpIfZero:
					if (getArgV!1.get!long == 0)
						goto case Jump;
					break;
				case JumpIfNotZero:
					if (getArgV!1.get!long != 0)
						goto case Jump;
					break;
				case And:
					regs_[instr.arg!0] = argCompareOp!(1, "&&", 2);
					break;
				case Or:
					regs_[instr.arg!0] = argCompareOp!(1, "||", 2);
					break;
				case Add:
					regs_[instr.arg!0] = argBinaryOp!(1, "+", 2);
					break;
				case Subtract:
					regs_[instr.arg!0] = argBinaryOp!(1, "-", 2);
					break;
				case Multiply:
					regs_[instr.arg!0] = argBinaryOp!(1, "*", 2);
					break;
				case Divide:
					regs_[instr.arg!0] = argBinaryOp!(1, "/", 2);
					break;
				case Remainder:
					regs_[instr.arg!0] = argBinaryOp!(1, "%", 2);
					break;
				case Power:
					regs_[instr.arg!0] = argBinaryOp!(1, "^^", 2);
					break;
				case Concat:
					regs_[instr.arg!0] = getArgV!1.concatOp(getArgV!2);
					break;
				case Test:
					regs_[instr.arg!0] = Value(getArgV!1.get!bool);
					break;
				case Equal:
					regs_[instr.arg!0] = argCompareOp!(1, "==", 2);
					break;
				case NotEqual:
					regs_[instr.arg!0] = argCompareOp!(1, "!=", 2);
					break;
				case Less:
					regs_[instr.arg!0] = argCompareOp!(1, "<", 2);
					break;
				case LessOrEqual:
					regs_[instr.arg!0] = argCompareOp!(1, "<=", 2);
					break;
				case Greater:
					regs_[instr.arg!0] = argCompareOp!(1, ">", 2);
					break;
				case GreaterOrEqual:
					regs_[instr.arg!0] = argCompareOp!(1, ">=", 2);
					break;
				case Length:
					regs_[instr.arg!0] = Value(getArgV!1.length);
					break;
				case Keys:
					regs_[instr.arg!0] = getArgV!1.keys();
					break;
				case TestKey:
					regs_[instr.arg!0] = Value(getArgV!1.has(getArgV!2));
					break;
				case Key:
					regs_[instr.arg!0] = getArgV!1.key(getArgV!2);
					break;
				case Slice:
					regs_[instr.arg!0] = getArgV!1.slice(getArgV!2, getArgV!3);
					break;
				case Dispatch:
					assert(!dispatchArg_);

					auto name = getArgV!2;
					auto pout = &regs_[instr.arg!0];

					auto obj = getArgV!1;
					switch (obj.type) with (Value.Type) {
					case Object:
					case AssocArray:
						if (getArgV!1.has(name, pout))
							break Lswitch;
						break;
					default:
						break;
					}

					if (name.type == Value.Type.String) {
						if (auto pvalue = name.get!string in globals_) {
							if (pvalue.type == Value.Type.Function) {
								dispatchArg_ = true;
								*pout = *pvalue;
								break Lswitch;
							}
						}
					}

					*pout = getArgV!1.get(name);
					break;
				case Element:
					regs_[instr.arg!0] = getArgV!1.get(getArgV!2);
					break;
				case LookUp:
					auto name = getArgV!1;
					auto pout = &regs_[instr.arg!0];

					foreach_reverse (ref s; scopes_) {
						if (s.has(name, pout))
							break Lswitch;
					}

					if (name.type == Value.Type.String) {
						if (auto pvalue = name.get!string in globals_) {
							*pout = *pvalue;
							break Lswitch;
						}
					}

					*pout = Value.init;
					break;
				case Call:
					auto func = getArgV!1;
					func.call(regs_[instr.arg!2..instr.arg!2 + instr.arg!3], regs_[instr.arg!0]);
					break;
				case DispatchCall:
					auto func = getArgV!1;
					if (dispatchArg_) {
						func.call(regs_[instr.arg!2..instr.arg!2 + instr.arg!3], regs_[instr.arg!0]);
						dispatchArg_ = false;
					} else {
						func.call(regs_[1 + instr.arg!2..instr.arg!2 + instr.arg!3], regs_[instr.arg!0]);
					}
					break;
				}

				if (++ip >= instrs_.length)
					break;
				instr = instrs_[ip];
			} catch (Exception e) {
				auto loc = locs_[ip];
				if (errorHandler_) {
					errorHandler_(Error(e.msg, sources_[loc.id], loc.line));
					break;
				} else {
					throw new VMException(e.msg, sources_[loc.id], loc.line);
				}
			}
		}
	}

private:
	Value[] consts_;
	Value[] regs_;
	Globals globals_;
	Value[] scopes_;
	bool dispatchArg_;

	const(Instr)[] instrs_;
	const(SourceLoc)[] locs_;
	const(string)[] sources_;

	ErrorHandler errorHandler_;
}
