module vayne.lib;


import std.algorithm;
import std.array;
import std.conv;

import vayne.value;


template bindVars(size_t i, alias Container, Vars...) {
	static if(i < Vars.length) {
		enum bindVars = Vars.length ? (__traits(identifier, Container) ~ "[\"" ~ __traits(identifier, Vars[i]) ~ "\"]=Value(Vars[" ~ i.to!string ~ "]);\n" ~ bindVars!(i + 1, Container, Vars)) : "";
	} else {
		enum bindVars = "";
	}
}


void bindLibDefault(ref Value[string] globals) {
	static long length(Value x) {
		return cast(long)x.length;
	}

	static bool empty(Value x) {
		return x.length == 0;
	}

	static Value keys(Value x) {
		return x.keys();
	}

	static Value def(Value x, Value def) {
		if (x.type == Value.Type.Undefined)
			return def;
		return x;
	}

	static string join(Value x, Value sep) {
		auto app = appender!string;
		auto len = x.length;
		auto seps = sep.get!string;
		foreach (size_t i, v; x) {
			app.put(v.get!string);
			if (i + 1 != len)
				app.put(seps);
		}
		return app.data;
	}

	static string[] split(Value x, Value sep) {
		return x.get!string.splitter(sep.get!string).array;
	}

	static long tointeger(Value x) {
		return x.get!long;
	}

	static double tofloat(Value x) {
		return x.get!double;
	}

	static string tostring(Value x) {
		return x.get!string;
	}

	static bool tobool(Value x) {
		return x.get!bool;
	}

	static string translate(Value[] x) {
		return x[0].get!string;
	}

	static string escape(Value[] x) {
		return x[0].get!string;
	}

	globals["length"] = Value(&length);
	globals["empty"] = Value(&empty);
	globals["keys"] = Value(&keys);

	globals["integer"] = Value(&tointeger);
	globals["float"] = Value(&tofloat);
	globals["string"] = Value(&tostring);
	globals["bool"] = Value(&tobool);

	globals["default"] = Value(&def);

	globals["join"] = Value(&join);
	globals["split"] = Value(&split);

	globals["__escape"] = Value(&escape);
	globals["__translate"] = Value(&translate);
}
