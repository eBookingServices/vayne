module templ;


import vibe.core.log;

import std.conv;
import std.functional;

import vayne.compiler;
import vayne.lib;
import vayne.serializer;
import vayne.vm;


void render(OutputStreamT, string FileName, Vars...)(OutputStreamT o__, string language__) {
	alias VayneVM = VM!();
	VayneVM vm;

	auto compiled = unserialize(cast(ubyte[])std.file.read("views/" ~ FileName ~ ".vayne"));

	Value[] constants;
	constants.reserve(compiled.constants.length);

	foreach(i, c; compiled.constants) {
		final switch (c.type) with (ConstantType) {
		case Boolean:
			constants ~= Value(c.value.to!bool);
			break;
		case Integer:
			constants ~= Value(c.value.to!long);
			break;
		case Float:
			constants ~= Value(c.value.to!double);
			break;
		case String:
			constants ~= Value(c.value.to!string);
			break;
		}
	}

	vm.load(compiled.registerCount, constants, compiled.instrs, compiled.locs, compiled.sources);

	VayneVM.Globals globals;

	bindLibDefault(globals);

	mixin(bindVars!(0, globals, Vars));

	auto translate(Value[] args) {
		assert(language__ == "en");
		auto tag = args[0].get!string;
		switch (tag) {
			case "footer":	return "This is the footer translation";
			case "empty":	return "Move along, nothing to see here";
			default: return tag;
		}
	}

	static errorHandler(VayneVM.Error error) {
		logInfo("%s(%s) template error: %s", error.source, error.line, error.msg);
	}

	globals["__translate"] = Value(&translate);
	vm.bindGlobals(globals);

	vm.errorHandler = toDelegate(&errorHandler);
	vm.execute(o__);
}
