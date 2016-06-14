module vayne.vm;


import std.conv;
import std.datetime;
import std.range;
import std.stdio;
import std.traits;


import vayne.op;
public import vayne.value;
import vayne.source.source;


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

	void load(size_t registers, size_t constants,const(Instr)[] instrs, const(SourceLoc)[] locs, const(string)[] sources) {
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

	@property void errorHandler(ErrorHandler handler) {
		errorHandler_ = handler;
	}

	@property ErrorHandler errorHandler() const {
		return errorHandler_;
	}

	void execute(T)(ref T output) if (isOutputRange!(T, char)) {
		Instr instr = instrs_[0];
		ulong ip;

		ref auto getArgV(size_t Arg)() if (Arg <= 2) {
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
				final switch (op) with (OpCode) {
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
				case Element:
					regs_[instr.arg!0] = getArgV!1.get(getArgV!2);
					break;
				case Call:
					auto func = getArgV!1.get!(Value.Function);
					func.wrapper(func.ptr, func.self, regs_[instr.arg!2..instr.arg!2 + instr.arg!3], regs_[instr.arg!0]);
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

	const(Instr)[] instrs_;
	const(SourceLoc)[] locs_;
	const(string)[] sources_;

	ErrorHandler errorHandler_;}
