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

	static Value values(Value x) {
		return x.values();
	}

	static Value front(Value x) {
		return x[0];
	}

	static Value back(Value x) {
		return x[x.length - 1];
	}

	static Value get(Value x, Value key, Value def) {
		Value result;
		if (x.has(key, &result))
			return result;
		return def;
	}

	static Value def(Value x, Value def) {
		if ((x.type == Value.Type.Undefined) || (x.type == Value.Type.Null))
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
		return x.toString();
	}

	static bool tobool(Value x) {
		return x.get!bool;
	}

	static string type(Value x) {
		final switch (x.type) with (Value.Type) {
		case Undefined:
			return "undefined";
		case Null:
			return "null";
		case Bool:
			return "bool";
		case Integer:
			return "integer";
		case Float:
			return "float";
		case Function:
			return "function";
		case String:
			return "string";
		case Array:
			return "array";
		case AssocArray:
			return "assocarray";
		case Object:
			return "object";
		case Pointer:
			return "pointer";
		}
	}

	static string escape(Value[] args) {
		auto value = args[0].get!string;

		foreach (f; args[1..$]) {
			switch (f.get!string) {
			case "html":
				value = escapeHTML(value);
				break;
			case "js":
				value = escapeJS(value);
				break;
			case "uri":
				value = escapeURI(value);
				break;
			default:
				assert("unimplemented escape filter: " ~ f.get!string);
				break;
			}
		}

		return value;
	}

	static string translate(Value[] args) {
		return args[0].get!string;
	}

	static string replace(string x, string from, string to) {
		return x.replace(from, to);
	}

	globals["length"] = Value(&length);
	globals["empty"] = Value(&empty);
	globals["keys"] = Value(&keys);
	globals["values"] = Value(&values);
	globals["front"] = Value(&front);
	globals["back"] = Value(&back);

	globals["integer"] = Value(&tointeger);
	globals["float"] = Value(&tofloat);
	globals["string"] = Value(&tostring);
	globals["bool"] = Value(&tobool);
	globals["type"] = Value(&type);

	globals["get"] = Value(&get);
	globals["default"] = Value(&def);

	globals["escape"] = Value(&escape);

	globals["replace"] = Value(&replace);

	globals["__escape"] = Value(&escape);
	globals["__translate"] = Value(&translate);
}


void bindLibMath(ref Value[string] globals) {
	import std.math;

	static Value abs(Value x) {
		final switch (x.type) with (Value.Type) {
		case Undefined:
		case Function:
		case String:
		case Array:
		case AssocArray:
		case Object:
		case Pointer:
		case Null:
		case Bool:
			return x;
		case Integer:
			return Value(std.math.abs(x.get!long));
		case Float:
			return Value(std.math.abs(x.get!double));
		}
	}

	static Value min(Value[] args) {
		size_t argMin;

		foreach (i; 1..args.length) {
			if (args[argMin].compareOp!">"(args[i]))
				argMin = i;
		}

		return args[argMin];
	}

	static Value max(Value[] args) {
		size_t argMax;

		foreach (i; 1..args.length) {
			if (args[argMax].compareOp!"<"(args[i]))
				argMax = i;
		}

		return args[argMax];
	}

	globals["abs"] = Value(&abs);
	globals["min"] = Value(&min);
	globals["max"] = Value(&max);
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

	static string lower(Value x) {
		return x.get!string.toLower();
	}

	static string upper(Value x) {
		return x.get!string.toUpper();
	}

	static long indexOf(Value[] args) {
		auto haystack = args[0].get!string;
		auto needle = args[1].get!string;
		auto start = (args.length > 2) ? args[2].get!size_t : 0;

		return haystack.indexOf(needle, start);
	}

	static string format(Value[] args) {
		auto fmt = args[0].get!string;
		auto spec = FormatSpec!char(fmt);

		auto app = appender!string;
		app.reserve(max(32, fmt.length + fmt.length >> 1));

		size_t arg = 1;
    	while (spec.writeUpToNextSpec(&app)) {
			if (arg >= args.length)
				break;

			auto value = args[arg++];
			final switch (value.type) with (Value.Type) {
			case Undefined:
			case Function:
			case String:
			case Array:
			case AssocArray:
			case Object:
			case Pointer:
				formatValue(&app, value.get!string, spec);
				break;
			case Null:
				formatValue(&app, null, spec);
				break;
			case Bool:
				formatValue(&app, value.get!bool, spec);
				break;
			case Integer:
				formatValue(&app, value.get!long, spec);
				break;
			case Float:
				formatValue(&app, value.get!double, spec);
				break;
			}
    	}

		if (arg != args.length)
			throw new Exception(std.format.format("number of arguments doesn't match number of format specifiers - expected %d, got %d", arg - 1, args.length - 1));
		return app.data;
	}

	globals["join"] = Value(&join);
	globals["split"] = Value(&split);
	globals["format"] = Value(&format);
	globals["strip"] = Value(&strip);
	globals["lower"] = Value(&lower);
	globals["upper"] = Value(&upper);
	globals["indexOf"] = Value(&indexOf);
}


void bindLibDefault(ref Value[string] globals) {
	bindLibBasic(globals);
	bindLibString(globals);
	bindLibMath(globals);
}


string escapeHTML(string x) {
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
			formattedWrite(&app, "&#x%02X;", cast(uint)ch);
			break;
		}
	}
	return app.data;
}


string escapeJS(string x) {
	auto app = appender!string;
	app.reserve(x.length + (x.length >> 1));

	foreach (ch; x.byDchar) {
		switch (ch) {
		case '\\':
			app.put(`\\`);
			break;
		case '\'':
			app.put(`\'`);
			break;
		case '\"':
			app.put(`\"`);
			break;
		case '\r':
			break;
		case '\n':
			app.put(`\n`);
			break;
		default:
			app.put(ch);
			break;
		}
	}
	return app.data;
}


string escapeURI(string x) {
	auto app = appender!string;
	app.reserve(8 + x.length + (x.length >> 1));

	foreach (i; 0..x.length) {
		switch (x.ptr[i]) {
		case 'A': .. case 'Z':
		case 'a': .. case 'z':
		case '0': .. case '9':
		case '-': case '_': case '.': case '~':
			app.put(x.ptr[i]);
			break;
		default:
			formattedWrite(&app, "%%%02X", x.ptr[i]);
			break;
		}
	}

	return app.data;
}
