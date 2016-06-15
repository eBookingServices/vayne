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


Instr encode(OpCode op, Args...)(Args args) if (Args.length <= 4) {
	static assert(op <= (OpMask >> OpShift));

	enum base = op << OpShift;

	final switch (op) with (OpCode) {
	case Nop:
	case Halt:
		static if (args.length == 0) {
			return Instr(base);
		} else {
			break;
		}
	case Jump:
	case PopScope:
		static if (args.length == 1) {
			assert(args[0].isImmediate);
			return Instr(base | (cast(ulong)args[0].value << Arg0Shift));
		} else {
			break;
		}
	case Increment:
	case Decrement:
	case Minus:
	case Not:
		static if (args.length == 1) {
			assert(args[0].isRegister);
			return Instr(base | (cast(ulong)args[0].value << Arg0Shift));
		} else {
			break;
		}
	case Output:
	case Throw:
	case PushScope:
		static if (args.length == 1) {
			assert(args[0].isRegisterOrConst);
			return Instr(base | (cast(ulong)args[0].value << Arg0Shift) | (args[0].isConst ? Arg0ConstMask : 0));
		} else {
			break;
		}
	case JumpIfZero:
	case JumpIfNotZero:
		static if (args.length == 2) {
			assert(args[0].isImmediate);
			assert(args[1].isRegisterOrConst);
			return Instr(base | (cast(ulong)args[0].value << Arg0Shift) | (cast(ulong)args[1].value << Arg1Shift) | (args[1].isConst ? Arg1ConstMask : 0));
		} else {
			break;
		}
	case Test:
	case Move:
	case Length:
	case Keys:
	case LookUp:
		static if (args.length == 2) {
			assert(args[0].isRegister);
			assert(args[1].isRegisterOrConst);
			return Instr(base | (cast(ulong)args[0].value << Arg0Shift) | (cast(ulong)args[1].value << Arg1Shift) | (args[1].isConst ? Arg1ConstMask : 0));
		} else {
			break;
		}
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
		static if (args.length == 3) {
			assert(args[0].isRegister);
			assert(args[1].isRegisterOrConst);
			assert(args[2].isRegisterOrConst);
			return Instr(base | (cast(ulong)args[0].value << Arg0Shift) | (cast(ulong)args[1].value << Arg1Shift) | (args[1].isConst ? Arg1ConstMask : 0) | (cast(ulong)args[2].value << Arg2Shift) | (cast(ulong)args[2].isConst ? Arg2ConstMask : 0));
		} else {
			break;
		}
	case Call:
	case DispatchCall:
		static if (args.length == 4) {
			assert(args[0].isRegister);
			assert(args[1].isRegisterOrConst);
			assert(args[2].isRegister);
			assert(args[3].isImmediate);
			assert(args[3].value <= ArgExMask);
			return Instr(base | (cast(ulong)args[0].value << Arg0Shift) | (cast(ulong)args[1].value << Arg1Shift) | (args[1].isConst ? Arg1ConstMask : 0) | (cast(ulong)args[2].value << Arg2Shift) | (cast(ulong)args[2].isConst ? Arg2ConstMask : 0) | (cast(ulong)args[3].value << ArgExShift));
		} else {
			break;
		}
	}

	assert(false, "wrong number of arguments to encode for opcode " ~ op.to!string);
}


enum {
	OpShift 		= 0,
	OpBitCount		= 6,
	OpMask 			= (1UL << OpBitCount) - 1,

	ArgBitCount 	= 17,

	Arg0Shift		= OpBitCount,
	Arg1Shift		= Arg0Shift + ArgBitCount,
	Arg2Shift		= Arg1Shift + ArgBitCount,

	Arg0Mask 		= (1UL << ArgBitCount) - 1,
	Arg1Mask 		= (1UL << ArgBitCount) - 1,
	Arg2Mask 		= (1UL << ArgBitCount) - 1,

	Arg0ConstMask	= 1UL << (Arg2Shift + ArgBitCount + 0),
	Arg1ConstMask	= 1UL << (Arg2Shift + ArgBitCount + 1),
	Arg2ConstMask	= 1UL << (Arg2Shift + ArgBitCount + 2),

	ArgExBitCount	= 4,
	ArgExShift		= Arg2Shift + ArgBitCount + 3,
	ArgExMask		= (1UL << ArgExBitCount) - 1,

	FlagsBitCount	= 4,
	FlagsShift		= Arg2Shift + ArgBitCount + 3,
	FlagsMask		= (1UL << FlagsBitCount) - 1,
}


struct Instr {
	ulong code;

	@property OpCode op() const {
		return cast(OpCode)((code >> OpShift) & OpMask);
	}

	auto arg(size_t Arg)() const {
		static if (Arg == 0) {
			return (code >> Arg0Shift) & Arg0Mask;
		} else static if (Arg == 1) {
			return (code >> Arg1Shift) & Arg1Mask;
		} else static if (Arg == 2) {
			return (code >> Arg2Shift) & Arg2Mask;
		} else static if (Arg == 3) {
			return (code >> ArgExShift) & ArgExMask;
		}
	}

	auto argConst(size_t Arg)() const {
		static if (Arg == 0) {
			return (code & Arg0ConstMask) != 0;
		} else static if (Arg == 1) {
			return (code & Arg1ConstMask) != 0;
		} else static if (Arg == 2) {
			return (code & Arg2ConstMask) != 0;
		} else static if (Arg == 3) {
			return false;
		}
	}

	auto argName(size_t Arg)() const {
		static if (Arg < 3) {
			return format("%s%s", (argConst!Arg ? "c" : "r"), arg!Arg);
		} else static if (Arg == 3) {
			return format("%s", arg!Arg);
		}
	}

	string toStringFull() const {
		return format("%016x %s", code, toString);
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
		case DispatchCall:
		case Call:
			return format("%s %s %s %s %s", name, argName!0, argName!1, argName!2, arg!3);
		}
	}
}
