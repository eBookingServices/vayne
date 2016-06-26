module vayne.lib;


import std.algorithm;
import std.array;
import std.conv;
import std.format;
import std.string;
import std.utf;


import vayne.value;


template bindVars(size_t i, alias Container, Vars...) {
	static if(i < Vars.length) {
		enum bindVars = Vars.length ? (__traits(identifier, Container) ~ "[\"" ~ __traits(identifier, Vars[i]) ~ "\"]=Value(Vars[" ~ i.to!string ~ "]);\n" ~ bindVars!(i + 1, Container, Vars)) : "";
	} else {
		enum bindVars = "";
	}
}


void bindLibBasic(ref Value[string] globals) {
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

	static string escape(Value[] x) {
		auto value = x[0].get!string;

		foreach (f; x[1..$]) {
			switch (f.get!string) {
			case "html":
				value = escapeHTML(value);
				break;
			default:
				assert("unimplemented escape filter: " ~ f.get!string);
				break;
			}
		}

		return value;
	}

	static string translate(Value[] x) {
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

	globals["escape"] = Value(&escape);

	globals["__escape"] = Value(&escape);
	globals["__translate"] = Value(&translate);
}


void bindLibString(ref Value[string] globals) {
	static string join(Value[] x) {
		auto app = appender!string;
		auto value = x[0];
		auto len = value.length;
		auto seps = (x.length > 1) ? x[1].get!string : ",";
		foreach (size_t i, v; value) {
			app.put(v.get!string);
			if (i + 1 != len)
				app.put(seps);
		}
		return app.data;
	}

	static string[] split(Value[] x) {
		auto value = x[0].get!string;
		auto sep = (x.length > 1) ? x[1].get!string : " ";

		return value.split(sep);
	}

	static string strip(Value x) {
		return x.get!string.strip;
	}

	static long indexOf(Value[] x) {
		auto haystack = x[0].get!string;
		auto needle = x[1].get!string;
		auto start = (x.length > 2) ? x[2].get!size_t : 0;

		return haystack.indexOf(needle, start);
	}

	static string toLower(Value x) {
		return x.get!string.toLower();
	}

	static string toUpper(Value x) {
		return x.get!string.toUpper();
	}

	globals["join"] = Value(&join);
	globals["split"] = Value(&split);
	globals["strip"] = Value(&strip);
	globals["indexOf"] = Value(&indexOf);
	globals["toLower"] = Value(&toLower);
	globals["toUpper"] = Value(&toUpper);

	globals["escapeHTML"] = Value(&escapeHTML);
}


void bindLibDefault(ref Value[string] globals) {
	bindLibBasic(globals);
	bindLibString(globals);
}


static string escapeHTML(string x) {
	auto app = appender!string;
	app.reserve(8 + x.length + (x.length >> 1));

	foreach (ch; x.byDchar) {
		switch (ch) {
		case '"':
			app.put("&quot;");
			break;
		case '\'':
			app.put("&#39;");
			break;
		case 'a': .. case 'z':
			goto case;
		case 'A': .. case 'Z':
			goto case;
		case '0': .. case '9':
			goto case;
		case ' ', '\t', '\n', '\r', '-', '_', '.', ':', ',', ';',
			'#', '+', '*', '?', '=', '(', ')', '/', '!',
			'%' , '{', '}', '[', ']', '$', '^', '~':
			app.put(cast(char)ch);
			break;
		case '<':
			app.put("&lt;");
			break;
		case '>':
			app.put("&gt;");
			break;
		case '&':
			app.put("&amp;");
			break;
		default:
			formattedWrite(&app, "&#%d;", cast(uint)ch);
			break;
		}
	}
	return app.data;
}
