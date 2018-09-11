module vayne.value;


import std.algorithm;
import std.array;
import std.conv;
import std.datetime;
import std.format;
import std.meta;
import std.range;
import std.traits;

import std.stdio;


import vayne.hash;


private struct IgnoreAttribute {}
private struct NameAttribute { const(char)[] name; }


@property IgnoreAttribute ignore() {
	return IgnoreAttribute();
}


@property NameAttribute bind(const(char)[] name)  {
	return NameAttribute(name);
}


private enum NotCallableNames = ["__ctor", "__xdtor", "__postblit", "__xpostblit", "opAssign", "opIndexAssign", "opCast", "opDollar", "opIndex", "opSlice", "opApply", "opCmp"];
private enum NotBindableNames = ["opIndex"];


private template isCompatibleStorageClass(size_t Class) {
	enum isCompatibleStorageClass = Class == 0;
}


private template isCompatibleArgType(T) {
	static if (is(T == enum)) {
		enum isCompatibleArgType = isCompatibleArgType!(OriginalType!T);
	} else {
		enum isCompatibleArgType = !isSomeFunction!T && (isScalarType!T || isSomeString!(OriginalType!T) || isBoolean!T || is(Unqual!T == Value) || (isArray!T && is(Unqual!(typeof(T.init[0])) == Value)) || (isAssociativeArray!T && is(Unqual!(KeyType!T) == Value) && is(Unqual!(ValueType!T) == Value)));
	}
}


private template isCompatibleReturnType(F) {
	alias R = ReturnType!F;
	enum isCompatibleReturnType = !isSomeFunction!R && ((isScalarType!R || isSomeString!(OriginalType!R) || isBoolean!R || is(Unqual!R == Value) || isArray!R || isAssociativeArray!R || is(R == class) || is(R == interface) || is(R == struct)));
}


private template isCompatibleFunction(T) {
	enum isCompatibleFunction = isCompatibleReturnType!T && allSatisfy!(isCompatibleArgType, ParameterTypeTuple!T) && allSatisfy!(isCompatibleStorageClass, ParameterStorageClassTuple!T);
}


private static void functionWrapper(T)(void* ptr, void* self, Value[] args, ref Value ret) if (isSomeFunction!T) {
	alias ParameterTypeTuple!T Args;

	static assert ((variadicFunctionStyle!T != Variadic.d && variadicFunctionStyle!T != Variadic.c), "Non-typesafe variadic functions are not supported.");

	static if (variadicFunctionStyle!T == Variadic.typesafe) {
		enum variadic = true;
		enum requiredArgs = Args.length - 1;
	} else {
		enum variadic = false;
		enum requiredArgs = Args.length;
	}

	static if (isFunctionPointer!T) {
		T func = cast(T)ptr;
	} else {
		T func;
		func.ptr = self;
		func.funcptr = cast(typeof(func.funcptr))ptr;
	}

	static if ((Args.length == 1) && isArray!(Args[0]) && is(Unqual!(ElementType!(Args[0])) == Value)) {
		auto argValues = args;
	} else {
		if ((variadic && (args.length < requiredArgs)) || (!variadic && args.length != requiredArgs))
			throw new Exception(format("expected %d arguments but got %d", requiredArgs, args.length));

		Args argValues;

		foreach (i, Arg; Args) {
			alias ArgU = Unqual!Arg;
			static if (is(Arg == enum)) {
				*cast(ArgU*)&argValues[i] = cast(ArgU)args[i].get!(Unqual!(OriginalType!ArgU));
			} else {
				*cast(ArgU*)&argValues[i] = args[i].get!ArgU;
			}
		}
	}

	static if (is(ReturnType!T == void)) {
		func(argValues);
	} else {
		ret = Value(func(argValues));
	}
}


struct Value {
	enum Type : ubyte {
		Undefined,
		Null,
		Bool,
		Integer,
		Float,
		Function,
		String,
		Array,
		AssocArray,
		Object,
		Pointer,
	}

	this(typeof(null)) {
		type_ = Type.Null;
	}

	this(Value x) {
		type_ = x.type_;
		storage_ = x.storage_;
	}

	this(T)(T x) if (isBoolean!T) {
		type_ = Type.Bool;
		storage_.b = x;
	}

	this(T)(T x) if (isScalarType!T && !isBoolean!T && !isFloatingPoint!T) {
		type_ = Type.Integer;
		storage_.l = cast(long)x;
	}

	this(T)(T x) if (isScalarType!T && !isBoolean!T && isFloatingPoint!T) {
		type_ = Type.Float;
		storage_.d = cast(double)x;
	}

	this(T)(T x) if (isSomeString!(OriginalType!T)) {
		type_ = Type.String;
		storage_.s = x;
	}

	this(T)(T x) if (isArray!T && !isSomeString!(OriginalType!T)) {
		type_ = Type.Array;
		static if (!is(Unqual!(ElementType!T) == Value)) {
			auto arr = uninitializedArray!(Value[])(x.length);
			static if (is(Unqual!(ElementType!T) == void)) {
				foreach (i, ref v; cast(ubyte[])x)
					arr[i] = Value(v);
			} else {
				foreach (i, ref v; x)
					arr[i] = Value(v);
			}
			storage_.a = arr;
		} else {
			storage_.a = cast(Value[])x;
		}
	}

	this(T)(T x) if (isSomeFunction!T && isCompatibleFunction!T) {
		if (x !is null) {
			type_ = Type.Function;
			static if (isFunctionPointer!T) {
				storage_.f.ptr = x;
			} else {
				storage_.f.self = x.ptr;
				storage_.f.ptr = cast(void*)x.funcptr;
			}
			storage_.f.wrapper = &functionWrapper!T;
		} else {
			type_ = Type.Null;
		}
	}

	this(T)(T x) if (isAssociativeArray!T) {
		type_ = Type.AssocArray;
		foreach (ref k, ref v; x)
			storage_.aa[Value(k)] = Value(v);
	}

	this(T)(T x) if (isPointer!T && !isSomeFunction!T) {
		static if (is(Unqual!(PointerTarget!T) == void) || isPointer!(PointerTarget!T)) {
			type_ = Type.Pointer;
			storage_.p = cast(void*)x;
		} else {
			if (x is null) {
				this(null);
			} else {
				this(*x);
			}
		}
	}

	this(T)(auto ref T x) if (is(Unqual!T == struct)) {
		static if (is(Unqual!(typeof(x.toVayneValue())) == Value)) {
			this = x.toVayneValue();
		} else {
			type_ = Type.Object;
			static if (__traits(isRef, x)) {
				bindMembers(x);
			} else {
				// struct have to be bound by ref because methods/delegates
				// might otherwise point to garbage memory
				bindMembers(*(new T) = x);
			}
		}
	}

	this(T)(T x) if (is(Unqual!T == class) || is(Unqual!T == interface)) {
		if (x !is null) {
			static if (is(Unqual!(typeof(x.toVayneValue())) == Value)) {
				this = x.toVayneValue();
			} else {
				type_ = Type.Object;
				bindMembers(x);
			}
		} else {
			type_ = Type.Null;
		}
	}

	private void bindMembers(T)(ref T x) {
		foreach (Member; FieldNameTuple!T) {
			static if (Member != "" && is(typeof(__traits(getMember, x, Member))) && __traits(getProtection, __traits(getMember, x, Member)) == "public" && !hasUDA!(__traits(getMember, T, Member), IgnoreAttribute)) {
				static if (hasUDA!(__traits(getMember, x, Member), NameAttribute)) {
					enum name = getUDAs!(__traits(getMember, x, Member), NameAttribute)[0].name;
				} else {
					enum name = Member;
				}

				static if (!isSomeFunction!(typeof(__traits(getMember, x, Member)))) {
					storage_.o[name] = Value(__traits(getMember, x, Member));
				} else static if (isCompatibleFunction!(typeof(__traits(getMember, x, Member)))) {
					if (__traits(getMember, x, Member) is !null) {
						storage_.o[name] = Value(__traits(getMember, x, Member));
					}
				}
			}
		}

		foreach (Member; __traits(derivedMembers, T)) {
			enum callable = !NotCallableNames.canFind(Member);
			enum getMember = "&(*cast(Unqual!(typeof(x))*)&x)." ~ Member;

			static if (is(FunctionTypeOf!(__traits(getMember, T, Member))) && (__traits(getProtection, __traits(getMember, x, Member)) == "public") && !hasUDA!(__traits(getMember, T, Member), IgnoreAttribute)) {
				static if (is(typeof(() { auto a = mixin(getMember); }))) {
					alias MemberType = typeof(mixin(getMember));
					static if (callable && isCompatibleFunction!MemberType) {
						enum bindable = !NotBindableNames.canFind(Member);

						alias Args = ParameterTypeTuple!MemberType;

						auto addr = mixin(getMember);

						static if (hasUDA!(__traits(getMember, x, Member), NameAttribute)) {
							enum name = getUDAs!(__traits(getMember, x, Member), NameAttribute)[0].name;
						} else {
							enum name = Member;
						}

						static if (bindable) {
							storage_.o[name] = Value(addr);
						}

						static if ((Member == "toString") && (Args.length == 0)) {
							storage_.o["__tostring"] = Value(addr);
						} else static if ((Member == "opIndex")) {
							//storage_.o["__index"] = Value(addr);
						}
					}
				}
			}
		}
	}

	@property Type type() const pure nothrow {
		return type_;
	}

	@property auto length() const {
		final switch(type) with (Type) {
		case Null:
		case Undefined:
		case Bool:
		case Integer:
		case Float:
		case Function:
		case Pointer:
			throw new Exception(format("length op not allowed for type %s", type));
		case String:
		case Array:
			return storage_.a.length;
		case AssocArray:
			return storage_.aa.length;
		case Object:
			return storage_.o.length;
		}
	}

	auto compareOp(string op)(in Value other) const {
		if (type != other.type) {
			if ((type == Type.Integer) && (other.type == Type.Float)) {
				return mixin("storage_.l " ~ op ~ " other.storage_.d");
			} else if ((type == Type.Float) && (other.type == Type.Integer)) {
				return mixin("storage_.d " ~ op ~ " other.storage_.l");
			}

			throw new Exception(format("compare op '%s' not allowed between types %s and %s", op, type, other.type));
		}

		final switch (type) with (Type) {
		case Null:
		case Undefined:
			return true;
		case Bool:
			return mixin("storage_.b" ~ op ~ "other.storage_.b");
		case Integer:
			return mixin("storage_.l " ~ op ~ " other.storage_.l");
		case Float:
			return mixin("storage_.d " ~ op ~ " other.storage_.d");
		case String:
			return mixin("storage_.s" ~ op ~ "other.storage_.s");
		case Function:
			return mixin("storage_.f.ptr" ~ op ~ "other.storage_.f.ptr");
		case Array:
			return mixin("storage_.a" ~ op ~ "other.storage_.a");
		case Pointer:
			return mixin("storage_.p" ~ op ~ "other.storage_.p");
		case AssocArray:
		case Object:
			break;
		}

		throw new Exception(format("compare op '%s' not allowed for type %s", op, type));
	}

	auto concatOp(in Value other) const {
		if (type != other.type || type != Type.String)
			throw new Exception(format("concat not allowed between types %s and %s", type, other.type));
		return Value(storage_.s ~ other.storage_.s);
	}

	auto binaryOp(string op)(Value other) {
		if (type != other.type) {
			if ((type == Type.Integer) && (other.type == Type.Float)) {
				return Value(mixin("storage_.l " ~ op ~ " other.storage_.d"));
			} else if ((type == Type.Float) && (other.type == Type.Integer)) {
				return Value(mixin("storage_.d " ~ op ~ " other.storage_.l"));
			}
			throw new Exception(format("binary op '%s' not allowed between types %s and %s", op, type, other.type));
		}

		final switch (type) with (Type) {
		case Bool:
			return Value(mixin("storage_.b " ~ op ~ " other.storage_.b"));
		case Integer:
			return Value(mixin("storage_.l " ~ op ~ " other.storage_.l"));
		case Float:
			return Value(mixin("storage_.d " ~ op ~ " other.storage_.d"));
		case Null:
		case Undefined:
		case String:
		case Function:
		case Array:
		case AssocArray:
		case Object:
		case Pointer:
			break;
		}

		throw new Exception(format("binary op '%s' not allowed for type %s", op, type));
	}

	void unaryOp(string op)() {
		final switch (type) with (Type) {
		case Integer:
			storage_.l = mixin(op ~ "storage_.l");
			break;
		case Float:
			storage_.d = mixin(op ~ "storage_.d");
			break;
		case Null:
		case Undefined:
		case Bool:
		case String:
		case Function:
		case Array:
		case AssocArray:
		case Object:
		case Pointer:
			throw new Exception(format("unary op '%s' not allowed for type %s", op, type));
		}
	}

	bool opEquals(const Value other) const {
		if (type_ == other.type_)
			return compareOp!("==")(other);
		return false;
	}

	size_t toHash() const nothrow @safe {
		return () @trusted {
			try {
				final switch (type) with (Type) {
				case Integer:
					return vayne.hash.hashOf(storage_.l);
				case Float:
					return vayne.hash.hashOf(storage_.d);
				case Null:
				case Undefined:
					return 0;
				case Bool:
					return vayne.hash.hashOf(storage_.b);
				case String:
					return vayne.hash.hashOf(storage_.s);
				case Function:
					return vayne.hash.hashOf(storage_.f);
				case Array:
					return vayne.hash.hashOf(storage_.a);
				case AssocArray:
					return vayne.hash.hashOf(storage_.aa);
				case Object:
					return vayne.hash.hashOf(storage_.o);
				case Pointer:
					return vayne.hash.hashOf(storage_.p);
				}
			} catch (Exception e) {
				return 0;
			}
		}();
	}

	string toString() const {
		final switch (type) with (Type) {
		case Undefined:
			return "undefined";
		case Null:
			return "null";
		case Bool:
			return storage_.b.to!string;
		case Integer:
			return storage_.l.to!string;
		case Float:
			return storage_.d.to!string;
		case String:
			return storage_.s.to!string;
		case Function:
			static if ((void*).sizeof == 4) {
				return format("[function 0x%08x:0x%08x]", storage_.f.self, storage_.f.ptr);
			} else {
				return format("[function 0x%016x:0x%016x]", storage_.f.self, storage_.f.ptr);
			}
		case Array:
			return storage_.a.to!string;
		case AssocArray:
			return storage_.aa.to!string;
		case Object:
			if (auto tostring = "__tostring" in storage_.o) {
				Value result;
				tostring.call(result, null);
				return result.get!string;
			}
			return storage_.o.to!string;
		case Pointer:
			static if ((void*).sizeof == 4) {
				return format("[pointer 0x%08x]", storage_.p);
			} else {
				return format("[pointer 0x%016x]", storage_.p);
			}
		}
	}

	T get(T)() const if (is(Unqual!T == Value)) {
		return this;
	}

	T get(T)() const if (isSomeString!(OriginalType!T)) {
		final switch (type) with (Type) {
		case Undefined:
		case Null:
		case Function:
		case Array:
		case AssocArray:
		case Pointer:
			throw new Exception(format("cannot convert %s to string", type));
		case Object:
			if (auto tostring = "__tostring" in storage_.o) {
				Value result;
				tostring.call(result, null);
				return result.get!T;
			}
			goto case AssocArray;
		case Bool:
			return storage_.b.to!T;
		case Integer:
			return storage_.l.to!T;
		case Float:
			return storage_.d.to!T;
		case String:
			return storage_.s.to!T;
		}
	}

	T get(T)() const if (isScalarType!T && !isBoolean!T) {
		final switch (type) with (Type) {
		case Bool:
			return cast(T)(storage_.b ? 1 : 0);
		case Integer:
			return cast(T)storage_.l;
		case Float:
			return cast(T)storage_.d;
		case String:
			return storage_.s.to!T;
		case Pointer:
			return cast(T)(cast(size_t)storage_.p);
		case Null:
		case Undefined:
		case Function:
		case Array:
		case AssocArray:
		case Object:
			throw new Exception(format("cannot convert %s to scalar", type));
		}
	}

	T get(T)() const if (isBoolean!T) {
		final switch (type) with (Type) {
		case Null:
			return false;
		case Bool:
			return storage_.b;
		case Integer:
			return storage_.l != 0;
		case Float:
			return storage_.d != 0.0;
		case String:
			return storage_.s.length != 0;
		case Pointer:
			return storage_.p != null;
		case Undefined:
		case Function:
		case Array:
		case AssocArray:
		case Object:
			throw new Exception(format("cannot convert %s to boolean", type));
		}
	}

	T get(T)() const if (isPointer!T) {
		final switch (type) with (Type) {
		case Null:
			return null;
		case Pointer:
			return cast(T)storage_.p;
		case Undefined:
		case Bool:
		case Integer:
		case String:
		case Float:
		case Function:
		case Array:
		case AssocArray:
		case Object:
			throw new Exception(format("cannot convert %s to boolean", type));
		}
	}

	T get(T)() const if (is(Unqual!T == Function)) {
		final switch (type) with (Type) {
		case Null:
		case Undefined:
		case Bool:
		case Integer:
		case Float:
		case String:
		case Array:
		case AssocArray:
		case Object:
		case Pointer:
			throw new Exception(format("cannot convert %s to function", type));
		case Function:
			return cast(T)storage_.f;
		}
	}

	T get(T)() const if (is(Unqual!T == Date)) {
		final switch (type) with (Type) {
		case Null:
		case Undefined:
		case Bool:
		case Integer:
		case Float:
		case String:
		case Array:
		case Pointer:
		case Function:
			throw new Exception(format("cannot convert %s to date", type));
		case Object:
			return Date(storage_.o["year"], storage_.o["month"], storage_.o["day"]);
		case AssocArray:
			return Date(storage_.aa["year"], storage_.aa["month"], storage_.aa["day"]);
		}
	}

	ref auto keys() const {
		final switch (type) with (Type) {
		case Null:
		case Undefined:
		case Bool:
		case Integer:
		case Float:
		case Function:
		case Pointer:
			throw new Exception(format("keys not allowed for type %s", type));
		case String:
		case Array:
			return Value(iota(0,length).array);
		case AssocArray:
			return Value(storage_.aa.keys);
		case Object:
			return Value(storage_.o.keys);
		}
	}

	ref auto values() const {
		final switch (type) with (Type) {
		case Null:
		case Undefined:
		case Bool:
		case Integer:
		case Float:
		case Function:
		case Pointer:
			throw new Exception(format("values not allowed for type %s", type));
		case String:
		case Array:
			return this;
		case AssocArray:
			return Value(storage_.aa.values);
		case Object:
			return Value(storage_.o.values);
		}
	}

	bool has(in Value index) const {
		final switch (type) with (Type) {
		case Null:
		case Undefined:
		case Bool:
		case Integer:
		case Float:
		case Function:
		case Pointer:
			throw new Exception(format("indexing not allowed for type %s, key '%s'", type, index));
		case String:
			auto i = index.get!ulong;
			return i < length;
		case Array:
			auto i = index.get!ulong;
			return (i < length);
		case AssocArray:
			auto i = index;
			return ((i in storage_.aa) != null);
		case Object:
			auto i = index.get!string;
			return ((i in storage_.o) != null);
		}
	}

	bool has(in Value index, Value* pout) const {
		final switch (type) with (Type) {
		case Null:
		case Undefined:
		case Bool:
		case Integer:
		case Float:
		case Function:
		case Pointer:
			throw new Exception(format("indexing not allowed for type %s, key '%s'", type, index));
		case String:
			auto i = index.get!size_t;
			if (i < length) {
				*pout = Value(storage_.s[i..i + 1]);
				return true;
			}
			return false;
		case Array:
			auto i = index.get!size_t;
			if (i < length) {
				*pout = (cast(Value*)storage_.a)[i];
				return true;
			}
			return false;
		case AssocArray:
			auto i = index;
			if (auto pvalue = i in storage_.aa) {
				*pout = *pvalue;
				return true;
			}
			return false;
		case Object:
			auto i = index.get!string;
			if (auto pvalue = i in storage_.o) {
				*pout = *pvalue;
				return true;
			}
			return false;
		}
	}

	Value opIndex(in Value index) const {
		final switch (type) with (Type) {
		case Null:
		case Undefined:
		case Bool:
		case Integer:
		case Float:
		case Function:
		case Pointer:
			throw new Exception(format("indexing not allowed for type %s, key '%s'", type, index));
		case String:
			auto i = index.get!size_t;
			if (i < length)
				return Value(storage_.s[i..i + 1]);
			throw new Exception(format("out of range index %d for length %d", i, length));
		case Array:
			auto i = index.get!size_t;
			if (i < length)
				return (cast(Value*)storage_.a)[i];
			throw new Exception(format("out of range index %d for length %d", i, length));
		case AssocArray:
			auto i = index;
			if (auto pvalue = i in storage_.aa)
				return *pvalue;
			throw new Exception(format("undefined key '%s' for associative array", i));
		case Object:
			auto i = index.get!string;
			if (auto pvalue = i in storage_.o)
				return *pvalue;
			throw new Exception(format("unknown member '%s' for object", i));
		}
	}

	Value opIndex(in string index) const {
		final switch (type) with (Type) {
		case Null:
		case Undefined:
		case Bool:
		case Integer:
		case Float:
		case Function:
		case Pointer:
			throw new Exception(format("indexing not allowed for type %s, key '%s'", type, index));
		case String:
			throw new Exception(format("indexing with string key not allowed for type %s, key '%s'", type, index));
		case Array:
			throw new Exception(format("indexing with string key not allowed for type %s, key '%s'", type, index));
		case AssocArray:
			auto i = Value(index);
			if (auto pvalue = i in storage_.aa)
				return *pvalue;
			throw new Exception(format("undefined key '%s' for associative array", i));
		case Object:
			if (auto pvalue = index in storage_.o)
				return *pvalue;
			throw new Exception(format("unknown member '%s' for object", index));
		}
	}

	Value opIndex(in size_t index) const {
		final switch (type) with (Type) {
		case Null:
		case Undefined:
		case Bool:
		case Integer:
		case Float:
		case Function:
		case Pointer:
			throw new Exception(format("indexing not allowed for type %s, key '%s'", type, index));
		case Object:
			throw new Exception(format("indexing with integer key not allowed for type %s, key '%s'", type, index));
		case String:
			if (index < length)
				return Value(storage_.s[index..index + 1]);
			throw new Exception(format("out of range index %d for length %d", index, length));
		case Array:
			if (index < length)
				return (cast(Value*)storage_.a)[index];
			throw new Exception(format("out of range index %d for length %d", index, length));
		case AssocArray:
			if (auto pvalue = Value(index) in storage_.aa)
				return *pvalue;
			throw new Exception(format("undefined key '%s' for associative array", index));
		}
	}

	void opIndexAssign(Value value, in string index) {
		final switch (type) with (Type) {
		case Null:
		case Undefined:
		case Bool:
		case Integer:
		case Float:
		case Function:
		case Pointer:
			throw new Exception(format("index assign not allowed for type %s, key '%s'", type, index));
		case String:
			throw new Exception(format("index assign with string key not allowed for type %s, key '%s'", type, index));
		case Array:
			throw new Exception(format("index assign with string key not allowed for type %s, key '%s'", type, index));
		case AssocArray:
			storage_.aa[Value(index)] = value;
			break;
		case Object:
			storage_.o[index] = value;
			break;
		}
	}

	void opIndexAssign(Value value, in size_t index) {
		final switch (type) with (Type) {
		case Null:
		case Undefined:
		case Bool:
		case Integer:
		case Float:
		case Function:
		case String:
		case Pointer:
			throw new Exception(format("index assign not allowed for type %s, key '%s'", type, index));
		case Object:
			throw new Exception(format("index assign with integer key not allowed for type %s, key '%s'", type, index));
		case Array:
			if (index < length) {
				storage_.a[index] = value;
				break;
			}
			throw new Exception(format("out of range index %d for length %d", index, length));
		case AssocArray:
			storage_.aa[Value(index)] = value;
			break;
		}
	}

	Value opSlice(in Value start, in Value end) const {
		return opSlice(start.get!size_t, end.get!size_t);
	}

	Value opSlice(in size_t start, in size_t end) const {
		final switch (type) with (Type) {
		case Null:
		case Undefined:
		case Bool:
		case Integer:
		case Float:
		case Function:
		case Pointer:
		case AssocArray:
		case Object:
			throw new Exception(format("slicing not allowed for type %s", type));
		case String:
			return Value(storage_.s[start..end]);
		case Array:
			return Value(storage_.a[start..end]);
		}
	}

	int opApply(int delegate(Value) dg) {
		final switch (type) with (Type) {
		case Null:
		case Undefined:
		case Bool:
		case Integer:
		case Float:
		case Function:
		case Pointer:
			throw new Exception(format("iteration not allowed for type %s", type));
		case String:
			foreach (v; storage_.s) {
				if (auto r = dg(Value(v)))
					return r;
			}
			break;
		case Array:
			foreach (v; storage_.a) {
				if (auto r = dg(v))
					return r;
			}
			break;
		case AssocArray:
			foreach (v; storage_.aa) {
				if (auto r = dg(v))
					return r;
			}
			break;
		case Object:
			foreach (v; storage_.o) {
				if (auto r = dg(v))
					return r;
			}
			break;
		}
		return 0;
	}

	int opApply(int delegate(size_t, Value) dg) {
		final switch (type) with (Type) {
		case Null:
		case Undefined:
		case Bool:
		case Integer:
		case Float:
		case Function:
		case Pointer:
			throw new Exception(format("iteration not allowed for type %s", type));
		case String:
			foreach (i, v; storage_.s) {
				if (auto r = dg(i, Value(v)))
					return r;
			}
			break;
		case Array:
			foreach (i, v; storage_.a) {
				if (auto r = dg(i, v))
					return r;
			}
			break;
		case AssocArray:
			foreach (i, v; storage_.aa) {
				if (auto r = dg(i.get!size_t, v))
					return r;
			}
			break;
		case Object:
			size_t i;
			foreach (v; storage_.o) {
				if (auto r = dg(i++, v))
					return r;
			}
			break;
		}
		return 0;
	}

	int opApply(int delegate(Value, Value) dg) {
		final switch (type) with (Type) {
		case Null:
		case Undefined:
		case Bool:
		case Integer:
		case Float:
		case Function:
		case Pointer:
			throw new Exception(format("iteration not allowed for type %s", type));
		case String:
			foreach (k, v; storage_.s) {
				if (auto r = dg(Value(k), Value(v)))
					return r;
			}
			break;
		case Array:
			foreach (k, v; storage_.a) {
				if (auto r = dg(Value(k), v))
					return r;
			}
			break;
		case AssocArray:
			foreach (k, v; storage_.aa) {
				if (auto r = dg(k, v))
					return r;
			}
			break;
		case Object:
			foreach (k, v; storage_.o) {
				if (auto r = dg(Value(k), v))
					return r;
			}
			break;
		}
		return 0;
	}

	int opApply(int delegate(string, Value) dg) {
		final switch (type) with (Type) {
		case Null:
		case Undefined:
		case Bool:
		case Integer:
		case Float:
		case Function:
		case Pointer:
			throw new Exception(format("iteration not allowed for type %s", type));
		case String:
			throw new Exception(format("iteration with string key not allowed for type %s", type));
		case Array:
			throw new Exception(format("iteration with string key not allowed for type %s", type));
		case AssocArray:
			foreach (k, v; storage_.aa) {
				if (auto r = dg(k.get!string, v))
					return r;
			}
			break;
		case Object:
			foreach (k, v; storage_.o) {
				if (auto r = dg(k, v))
					return r;
			}
			break;
		}
		return 0;
	}

	static auto emptyObject() {
		Value value;
		value.type_ = Type.Object;
		return value;
	}

	static auto emptyAssocArray() {
		Value value;
		value.type_ = Type.AssocArray;
		return value;
	}

	void call(ref Value ret, Value[] args) const {
		auto func = get!Function();
		func.wrapper(func.ptr, func.self, args, ret);
	}

	void call(Args...)(ref Value ret, Args args) const {
		Value[Args.length] argValues = [ args ];
		auto func = get!Function();
		func.wrapper(func.ptr, func.self, argValues, ret);
	}

	static struct Function {
		void* self;
		void* ptr;
		void function(void* ptr, void* self, Value[], ref Value) wrapper;
	}

	union Storage {
		bool b;
		long l;
		double d;
		string s;
		Value[] a;
		Value[Value] aa;
		Value[string] o; // TODO: use a custom hash map for proper inlining and access to internals
		Function f;
		void* p;
	}

	private Type type_;
	private Storage storage_;
}
