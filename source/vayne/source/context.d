module vayne.source.context;


import std.format;

import vayne.source.source;
import vayne.source.token;


class ContextException : Exception {
	this(SourceLoc loc, string msg) {
		super(msg);

		this.loc = loc;
	}

	SourceLoc loc;
}


class Context {
	this(Source source) {
		this.source = source;
		this.loc = SourceLoc(source.id, 1);
	}

	this(Source source, SourceLoc loc) {
		this.source = source;
		this.loc = loc;
	}

	auto advance(size_t offset) {
		loc.line += source.buffer[cursor..cursor + offset].countLines;
		cursor += offset;
		return this;
	}

	auto remaining() {
		return source.buffer[cursor..$];
	}

	auto open(string tag, string content) {
		opens_ ~= Open(loc, cursor, tag, content);
	}

	auto close() {
		if (opens_.length == 0)
			throw new ContextException(loc, "unexpected '/' tag; no tag is open");
		auto tag = opens_[$ - 1];
		opens_ = opens_[0..$-1];
		return tag;
	}

	auto expectClosed() {
		if (opens_.length) {
			auto lastOpen = opens_[$-1];
			throw new ContextException(lastOpen.loc, format("unexpected end of file; missing '/' tag for '%s'", lastOpen.tag));
		}
	}

	auto expectOpen(string tag, string related) {
		if (!opens_.length || opens_[$-1].tag != tag)
			throw new ContextException(loc, format("unexpected '%s' tag; no '%s' tag in open", related, tag));
	}

	Source source;
	SourceLoc loc;
	size_t cursor;

	static struct Open {
		SourceLoc loc;
		size_t cursor;
		string tag;
		string content;
	}

	private Open[] opens_;
}
