import std.algorithm;
import std.array;
import std.datetime;
import std.file;
import std.getopt;
import std.path;
import std.stdio;
import std.string;

import vayne.ast.node;
import vayne.ast.printer;
import vayne.code.emitter;
import vayne.code.serializer;
import vayne.source.compress;
import vayne.source.parser;
import vayne.source.preparser;
import vayne.source.source;


struct CompileOptions {
	auto preparsePrint = false;
	auto astPrint = false;
	auto instrPrint = false;
	auto constPrint = false;
	auto lineNumbers = false;
	auto compress = false;

	string[] search;
	string ext = ".html";
}

const(string)[] compile(string fileName, string target, CompileOptions options) {
	SourceManagerOptions mgrOptions;
	mgrOptions.search ~= options.search;
	mgrOptions.extension = options.ext;

	auto mgr = SourceManager(mgrOptions);
	auto src = mgr.open(fileName);

	PreParserOptions preOptions;
	preOptions.lineNumbers = options.lineNumbers;

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
			writeln(format("%4d %8s %s", i, k.type, k.value));
	}

	if (options.instrPrint) {
		if (options.lineNumbers) {
			foreach (ip, instr; emitter.instrs)
				writeln(format("%4d %s ; %s", ip, instr, mgr.loc(emitter.locs[ip])));
		} else {
			foreach (ip, instr; emitter.instrs)
				writeln(format("%4d %s", ip, instr));
		}
	}

	auto byteCode = serialize(emitter, mgr.sourceNames);

	std.file.write(target, byteCode);

	return mgr.dependencies.map!(x => mgr.fileNames[x]).array;
}


int main(string[] args) {
	version (linux) {
		import etc.linux.memoryerror;
		registerMemoryErrorHandler();
	}

	auto result = 0;
	auto time = false;
	auto verbose = false;
	auto depCacheGenOnly = false;
	string outputDir;
	string depCacheDir;

	CompileOptions compileOptions;

	try {
		auto opts = getopt(args,
			"r|print-preparse",	"print preparser result", &compileOptions.preparsePrint,
			"a|print-ast",		"print ast", &compileOptions.astPrint,
			"i|print-instrs",	"print generated instructions", &compileOptions.instrPrint,
			"c|print-consts",	"print generated constant slots", &compileOptions.constPrint,
			"t|time",			"display elapsed time", &time,
			"v|verbose",		"verbose output", &verbose,
			"l|line-numbers",	"keep accurate line number information (disables compression)", &compileOptions.lineNumbers,
			"c|compress",		"compress HTML in between template tags", &compileOptions.compress,
			"o|output-dir",		"output directory", &outputDir,
			"d|dep-cache-dir",	"dependant-cache directory", &depCacheDir,
			"g|dep-gen-only",	"only generate dependant-cache, do not re-compile dependants", &depCacheGenOnly,
			"j|search",			"search path(s) to look for source files", &compileOptions.search,
			"e|default-ext", 	"default source file extension (defaults to .html)", &compileOptions.ext);

		if (opts.helpWanted || (args.length != 2)) {
			defaultGetoptPrinter("Usage: vayne [OPTIONS] file\n", opts.options);
			return 1;
		}
	} catch (Exception e) {
		writeln(e.msg);
		return 1;
	}

	if (compileOptions.lineNumbers)
		compileOptions.compress = false;

	if (!outputDir.empty) {
		if (!isAbsolute(outputDir))
			outputDir = absolutePath(outputDir);
	}

	auto fileName = args[1];
	if (verbose)
		writeln("compiling ", fileName, "...");

	auto timeStart = Clock.currTime;

	try {
		auto target = buildNormalizedPath(outputDir, fileName ~ ".vayne");

		if (!outputDir.empty) {
			try {
				mkdirRecurse(outputDir);
			} catch(Throwable) {
			}
		}

		auto deps = compile(fileName, target, compileOptions);

		if (!depCacheDir.empty) {
			immutable depsExtension = ".deps";

			auto depsFileName = buildNormalizedPath(depCacheDir, fileName ~ depsExtension);
			try {
				mkdirRecurse(depsFileName.dirName);
			} catch(Throwable) {
			}

			if (!depCacheGenOnly) {
				if (!isAbsolute(depCacheDir))
					depCacheDir = absolutePath(depCacheDir);

				string[] dependants;
				foreach (entry; dirEntries(depCacheDir, SpanMode.breadth)) {
					if (!entry.isDir) {
						if (entry.name.extension == depsExtension) {
							foreach (depName; File(entry.name).byLine) {
								if (depName == fileName)
									dependants ~= relativePath(entry.name[0..$ - depsExtension.length], depCacheDir);
							}
						}
					}
				}

				if (dependants.length) {
					foreach(dependant; dependants) {
						if (verbose)
							writeln("compiling dependant ", dependant, "...");

						compile(dependant, outputDir, compileOptions);
					}
				}
			}

			Appender!string appender;
			appender.reserve(2 * 1024);
			foreach (depName; deps) {
				appender.put(depName);
				appender.put("\n");
			}

			std.file.write(depsFileName, appender.data);
		}
	} catch (Exception error) {
		writeln("error: ", error.msg);
		result = 1;
	}

	auto timeEnd = Clock.currTime;
	if (time)
		writeln(format("elapsed: %.1fms", (timeEnd - timeStart).total!"usecs" * 0.001f));

	return result;
}
