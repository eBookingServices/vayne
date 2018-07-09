module vayne.compiler;


import std.algorithm;
import std.array;
import std.file;
import std.path;
import std.stdio;
import std.string;

import vayne.ast.node;
import vayne.ast.printer;
import vayne.code.emitter;
import vayne.op;
import vayne.serializer;
import vayne.source.compress;
import vayne.source.parser;
import vayne.source.preparser;
import vayne.source.source;


struct CompilerOptions {
	auto preparsePrint = false;
	auto astPrint = false;
	auto instrPrint = false;
	auto byteCodePrint = false;
	auto constPrint = false;
	auto lineNumbers = true;
	auto compress = false;
	auto verboseIncludes = false;

	string[] search;
	string ext = ".html";
}


enum ConstantType : ubyte {
	Null,
	Boolean,
	String,
	Integer,
	Float,
}


struct CompiledCode {
	uint registerCount;

	const(Instr)[] instrs;
	const(SourceLoc)[] locs;
	const(string)[] sources;
	const(Constant)[] constants;
	const(string)[] dependencies;

	static struct Constant {
		ConstantType type;
		string value;
	}
}


class CompileErrorsException : Exception {
	this(string[] errors) {
		assert(!errors.empty);
		super(errors[0]);

		this.errors = errors;
	}

	string[] errors;
}


CompiledCode compile(string fileName, CompilerOptions options) {
	try {
		SourceManagerOptions mgrOptions;
		mgrOptions.search ~= options.search;
		mgrOptions.extension = options.ext;

		auto mgr = SourceManager(mgrOptions);
		auto src = mgr.open(fileName);

		PreParserOptions preOptions;
		preOptions.lineNumbers = options.lineNumbers;
		preOptions.verboseIncludes = options.verboseIncludes;
		mgr.set(src.id, preparse(mgr, src.id, preOptions));

		if (options.preparsePrint)
			writeln(mgr.get(src.id).buffer);

		ParserOptions parserOptions;
		parserOptions.compress = options.compress ? CompressOptions.defaults : CompressOptions.none;

		auto ast = parse(mgr, src.id, parserOptions);
		if (options.astPrint)
			ast.print.writeln;

		Emitter emitter;
		emitter.emitModule(cast(Module)ast);

		if (options.constPrint) {
			foreach (i, k; emitter.constants)
				writeln(format("%5d %-16s %s", i, k.type, k.value));
		}

		if (options.instrPrint || options.byteCodePrint) {
			auto fmt = "%5d " ~ (options.byteCodePrint ? "%-58s" : "%-24s");

			if (options.lineNumbers)
				fmt ~= " ; %s";

			if (options.lineNumbers) {
				foreach (ip, instr; emitter.instrs)
					writeln(format(fmt, ip, options.byteCodePrint ? instr.toStringFull : instr.toString, mgr.loc(emitter.locs[ip])));
			} else {
				foreach (ip, instr; emitter.instrs)
					writeln(format(fmt, ip, options.byteCodePrint ? instr.toStringFull : instr.toString));
			}
		}

		CompiledCode result;
		with (result) {
			registerCount = emitter.registerCount;

			instrs = emitter.instrs;
			locs = emitter.locs;
			sources = mgr.sourceNames;
			dependencies = mgr.dependencies.map!(x => mgr.fileNames[x]).array;

			constants.reserve(emitter.constants.length);
			foreach (i, k; emitter.constants) {
				final switch (k.type) with (Emitter.ConstantSlot.Type) {
				case Null:
					constants ~= CompiledCode.Constant(ConstantType.Null, k.value);
					break;
				case Boolean:
					constants ~= CompiledCode.Constant(ConstantType.Boolean, k.value);
					break;
				case Integer:
					constants ~= CompiledCode.Constant(ConstantType.Integer, k.value);
					break;
				case Float:
					constants ~= CompiledCode.Constant(ConstantType.Float, k.value);
					break;
				case String:
					constants ~= CompiledCode.Constant(ConstantType.String, k.value);
					break;
				}
			}
		}

		return result;
	} catch (Exception error) {
		string[] errors;

		if (auto parserErrors = cast(ParserErrorsException)error) {
			errors = parserErrors.errors;
		} else {
			errors ~= error.msg;
		}

		throw new CompileErrorsException(errors);
	}
}
