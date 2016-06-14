module vayne.source.source;


import std.file;
import std.format;
import std.path;


static struct SourceManagerOptions {
	string[] search;
	string extension = ".html";
}

struct SourceManager {
	@disable this();

	this(SourceManagerOptions options) {
		options_ = options;
	}

	Source add(string name, string buffer, uint parent = uint.max, string fileName = null) {
		if (auto pindex = name in index_)
			return sources_[*pindex];

		auto id = cast(uint)sources_.length;

		sourceNames_ ~= name;
		fileNames_ ~= fileName.length ? fileName : name;
		sources_ ~= Source(id, parent, buffer);
		index_[name] = id;

		return sources_[id];
	}

	void set(uint id, string buffer) {
		sources_[id].buffer = buffer;
	}

	void dependency(uint id, uint dependency, SourceLoc loc) {
		if (id == dependency)
			throw new Exception(format("source '%s' directly depends on itself - triggered at %s", sourceNames_[id], this.loc(loc)));

		auto source = sources_[id];
		while (source.parent != uint.max) {
			if (source.parent == dependency)
				throw new Exception(format("cyclic dependency with source '%s' triggered at %s", sourceNames_[dependency], this.loc(loc)));
			source = sources_[source.parent];
		}

		foreach (dep; deps_) {
			if (dep == dependency)
				return;
		}

		deps_ ~= dependency;
	}

	auto get(uint id) {
		return sources_[id];
	}

	auto name(uint id) {
		return sourceNames_[id];
	}

	auto loc(SourceLoc loc) {
		return format("%s(%d)", name(loc.id), loc.line);
	}

	auto open(string fileName, bool binary = false, uint parent = uint.max) {
		if (auto pindex = fileName in index_)
			return sources_[*pindex];

		auto name = resolvePath(fileName);
		if (!name.length && !name.extension.length)
			name = resolvePath(fileName ~ options_.extension);

		if (!name.length)
			throw new Exception(format("file not found '%s'", fileName));

		auto bytes = cast(string)read(name);

		return add(name, binary ? bytes.stripUTFbyteOrderMarker : bytes, parent, name);
	}

	auto resolvePath(string fileName) {
		static string checkSearch(string[] search, string fileName) {
			foreach (path; search) {
				auto name = buildNormalizedPath(path, fileName);
				if (exists(name))
					return name;
			}
			return null;
		}

		if (exists(fileName))
			return fileName;
		return checkSearch(options_.search, fileName);
	}

	auto sourceNames() const {
		return sourceNames_;
	}

	auto fileNames() const {
		return fileNames_;
	}

	auto dependencies() const {
		return deps_;
	}

private:
	uint[string] index_;
	string[] sourceNames_;
	string[] fileNames_;
	Source[] sources_;
	uint[] deps_;

	SourceManagerOptions options_;
}


struct Source {
	uint id;
	uint parent;
	string buffer;

	@property auto ptr() const {
		return buffer.ptr;
	}

	@property auto ptr(T)(T* ptr) {
		assert(ptr >= buffer.ptr && ptr <= end);
		buffer = buffer[ptr - buffer.ptr..$];
	}

	@property auto end() const {
		return buffer.ptr + buffer.length;
	}

	@property auto empty() const {
		return buffer.length == 0;
	}
}


struct SourceLoc {
	uint id;
	uint line;
	uint column;
}


bool balancedQuotes(Range)(Range range) {
	char open = '\0';
	char last = '\0';
	foreach(ch; range) {
		if (last != '\\') {
			switch(ch) {
			case '"':
				open = (open == '"') ? '\0' : '"';
				break;
			case '\'':
				open = (open == '\'') ? '\0' : '\'';
				break;
			case '`':
				open = (open == '`') ? '\0' : '`';
				break;
			default:
				break;
			}
		}
		last = ch;
	}
	return open == '\0';
}


size_t countLines(string x) {
	size_t count;
	foreach(i; 0..x.length)
		count += (x.ptr[i] == '\n');
	return count;
}


string stripUTFbyteOrderMarker(string x) {
	if (x.length >= 3 && (x[0] == 0xef) && (x[1] == 0xbb) && (x[2] == 0xbf))
		return x[3..$];
	return x;
}
