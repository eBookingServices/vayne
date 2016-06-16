module vayne.op;


import std.conv;
import std.format;
import std.meta;


enum : uint { OpCodesVersion = 1 };


enum OpCode : ulong {
	Nop,
	Halt,
	Throw,
	Output,

	Jump,
	JumpIfZero,
	JumpIfNotZero,

	Move,
	PushScope,
	PopScope,

	Increment,
	Decrement,
	Minus,
	Not,

	And,
	Or,
	Add,
	Subtract,
	Multiply,
	Divide,
	Remainder,
	Power,
	Concat,

	Test,
	Equal,
	NotEqual,
	Less,
	LessOrEqual,
	Greater,
	GreaterOrEqual,

	Length,
	Keys,
	TestKey,
	Key,
	Slice,
	Dispatch,
	Element,
	LookUp,

	Call,
	DispatchCall,
}


struct Operand {
	enum Kind : uint {
		Immediate = 0,
		Register,
		Constant,
	}

	auto isConst() const {
		return kind == Kind.Constant;
	}

	auto isImmediate() const {
		return kind == Kind.Immediate;
	}

	auto isRegister() const {
		return kind == Kind.Register;
	}

	auto isRegisterOrConst() const {
		return (kind == Kind.Register) || (kind == Kind.Constant);
	}

	uint kind;
	uint value;
}


// code0: [63 arg1 const?][62 arg0 const?][60..33 arg1][33..6 arg0][6..0 op]
// code1: [63 arg3 const?][62 arg2 const?][62..54 unused][54..27 arg3][27..0 arg2]

enum {
	OpBitCount		= 6,
	OpMask 			= (1UL << OpBitCount) - 1,

	ArgBitCount 	= 28,

	Arg0Shift		= OpBitCount,
	Arg1Shift		= Arg0Shift + ArgBitCount,
	Arg2Shift		= 0,
	Arg3Shift		= Arg2Shift + ArgBitCount,

	ArgMask 		= (1UL << ArgBitCount) - 1,

	Arg0ConstMask	= 1UL << 62,
	Arg1ConstMask	= 1UL << 63,
	Arg2ConstMask	= 1UL << 62,
	Arg3ConstMask	= 1UL << 63,
}


struct Instr {
	this(ulong code0, ulong code1) {
		this.code0 = code0;
		this.code1 = code1;
	}

	this(Args...)(OpCode op, Args args) {
		assert(op <= OpMask);

		auto argCountValid = false;

		enum argCount = args.length;

		final switch (op) with (OpCode) {
		case Nop:
		case Halt:
			static if (argCount == 0) {
				argCountValid = true;
			}
			break;
		case Jump:
		case PopScope:
			static if (argCount == 1) {
				assert(args[0].isImmediate);
				argCountValid = true;
			}
			break;
		case Increment:
		case Decrement:
		case Minus:
		case Not:
			static if (argCount == 1) {
				assert(args[0].isRegister);
				argCountValid = true;
			}
			break;
		case Output:
		case Throw:
		case PushScope:
			static if (argCount == 1) {
				assert(args[0].isRegisterOrConst);
				argCountValid = true;
			}
			break;
		case JumpIfZero:
		case JumpIfNotZero:
			static if (argCount == 2) {
				assert(args[0].isImmediate);
				assert(args[1].isRegisterOrConst);
				argCountValid = true;
			}
			break;
		case Test:
		case Move:
		case Length:
		case Keys:
		case LookUp:
			static if (argCount == 2) {
				assert(args[0].isRegister);
				assert(args[1].isRegisterOrConst);
				argCountValid = true;
			}
			break;
		case And:
		case Or:
		case Add:
		case Subtract:
		case Multiply:
		case Divide:
		case Remainder:
		case Power:
		case Concat:
		case Equal:
		case NotEqual:
		case Less:
		case LessOrEqual:
		case Greater:
		case GreaterOrEqual:
		case TestKey:
		case Key:
		case Dispatch:
		case Element:
			static if (argCount == 3) {
				assert(args[0].isRegister);
				assert(args[1].isRegisterOrConst);
				assert(args[2].isRegisterOrConst);
				argCountValid = true;
			}
			break;
		case Slice:
			static if (argCount == 4) {
				assert(args[0].isRegister);
				assert(args[1].isRegisterOrConst);
				assert(args[2].isRegisterOrConst);
				assert(args[3].isRegisterOrConst);
				argCountValid = true;
			}
			break;
		case Call:
		case DispatchCall:
			static if (argCount == 4) {
				assert(args[0].isRegister);
				assert(args[1].isRegisterOrConst);
				assert(args[2].isRegister);
				assert(args[3].isImmediate);
				argCountValid = true;
			}
		}

		if (!argCountValid)
			assert(false, "wrong number of arguments to encode for opcode " ~ op.to!string);

		code0 = op;

		static if (argCount > 0) {
			assert(args[0].value <= ArgMask);
			code0 |= (cast(ulong)args[0].value << Arg0Shift) | (args[0].isConst ? Arg0ConstMask : 0);
		}
		static if (argCount > 1) {
			assert(args[1].value <= ArgMask);
			code0 |= (cast(ulong)args[1].value << Arg1Shift) | (args[1].isConst ? Arg1ConstMask : 0);
		}
		static if (argCount > 2) {
			assert(args[2].value <= ArgMask);
			code1 |= (cast(ulong)args[2].value << Arg2Shift) | (args[2].isConst ? Arg2ConstMask : 0);
		}
		static if (argCount > 3) {
			assert(args[3].value <= ArgMask);
			code1 |= (cast(ulong)args[3].value << Arg3Shift) | (args[3].isConst ? Arg3ConstMask : 0);
		}
	}

	ulong code0;
	ulong code1;

	@property OpCode op() const {
		return cast(OpCode)(code0 & OpMask);
	}

	auto arg(size_t Arg)() const {
		static if (Arg == 0) {
			return (code0 >> Arg0Shift) & ArgMask;
		} else static if (Arg == 1) {
			return (code0 >> Arg1Shift) & ArgMask;
		} else static if (Arg == 2) {
			return (code1 >> Arg2Shift) & ArgMask;
		} else static if (Arg == 3) {
			return (code1 >> Arg3Shift) & ArgMask;
		}
	}

	auto argConst(size_t Arg)() const {
		static if (Arg == 0) {
			return (code0 & Arg0ConstMask) != 0;
		} else static if (Arg == 1) {
			return (code0 & Arg1ConstMask) != 0;
		} else static if (Arg == 2) {
			return (code1 & Arg2ConstMask) != 0;
		} else static if (Arg == 3) {
			return (code1 & Arg3ConstMask) != 0;
		}
	}

	auto argName(size_t Arg)() const {
		return format("%s%s", (argConst!Arg ? "c" : "r"), arg!Arg);
	}

	string toStringFull() const {
		return format("%016x%016x %s", code1, code0, toString);
	}

	bool isBoolean() const {
		final switch (op) with (OpCode) {
		case Nop:
		case Halt:
		case Jump:
		case Increment:
		case Decrement:
		case Minus:
		case Output:
		case Throw:
		case JumpIfZero:
		case JumpIfNotZero:
		case Move:
		case Length:
		case Keys:
		case And:
		case Or:
		case Add:
		case Subtract:
		case Multiply:
		case Divide:
		case Remainder:
		case Power:
		case Concat:
		case Key:
		case Slice:
		case Dispatch:
		case Element:
		case LookUp:
		case Call:
		case DispatchCall:
		case PushScope:
		case PopScope:
			return false;
		case Not:
		case Test:
		case Equal:
		case NotEqual:
		case Less:
		case LessOrEqual:
		case Greater:
		case GreaterOrEqual:
		case TestKey:
			return true;
		}
	}

	string toString() const {
		auto name = op.to!string;

		final switch (op) with (OpCode) {
		case Nop:
		case Halt:
			return name;
		case Jump:
		case PopScope:
			return format("%s %s", name, arg!0);
		case Increment:
		case Decrement:
		case Minus:
		case Not:
		case Output:
		case Throw:
		case PushScope:
			return format("%s %s", name, argName!0);
		case JumpIfZero:
		case JumpIfNotZero:
			return format("%s %s %s", name, arg!0, argName!1);
		case Test:
		case Move:
		case Length:
		case Keys:
		case LookUp:
			return format("%s %s %s", name, argName!0, argName!1);
		case And:
		case Or:
		case Add:
		case Subtract:
		case Multiply:
		case Divide:
		case Remainder:
		case Power:
		case Concat:
		case Equal:
		case NotEqual:
		case Less:
		case LessOrEqual:
		case Greater:
		case GreaterOrEqual:
		case TestKey:
		case Key:
		case Dispatch:
		case Element:
			return format("%s %s %s %s", name, argName!0, argName!1, argName!2);
		case Slice:
			return format("%s %s %s %s %s", name, argName!0, argName!1, argName!2, argName!3);
		case DispatchCall:
		case Call:
			return format("%s %s %s %s %s", name, argName!0, argName!1, argName!2, arg!3);
		}
	}
}
