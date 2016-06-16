module vayne.serializer;


import std.algorithm;
import std.range;
import std.traits;

import vayne.compiler;
import vayne.op;
import vayne.source.source;


const(ubyte)[] serialize(CompiledCode code) {
	ByteStreamWriter writer;

	with (writer) {
		put!uint(MagicString);
		put!uint(FormatVersion);
		put!uint(OpCodesVersion);
		put!uint(code.registerCount);

		if (code.constants.length) {
			put!ubyte(Chunks.Constants);
			put!uint(cast(uint)code.constants.length);
			foreach (i, k; code.constants) {
				put!ubyte(k.type);
				put!string(k.value);
			}
		}

		if (code.instrs.length) {
			put!ubyte(Chunks.Instructions);
			put!uint(cast(uint)code.instrs.length);
			foreach (instr; code.instrs) {
				put!ulong(instr.code0);
				put!ulong(instr.code1);
			}
		}

		if (code.locs.length) {
			put!ubyte(Chunks.InstructionSourceLocations);
			put!uint(cast(uint)code.locs.length);
			foreach (loc; code.locs) {
				put!uint(loc.id);
				put!uint(loc.line);
				//writer.put!uint(loc.column);
			}
		}

		if (code.sources.length) {
			put!ubyte(Chunks.SourceNames);
			put!uint(cast(uint)code.sources.length);
			foreach (source; code.sources)
				put!string(source);
		}

		if (false && code.dependencies.length) {
			put!ubyte(Chunks.Dependencies);
			put!uint(cast(uint)code.dependencies.length);
			foreach (dependency; code.dependencies)
				put!string(dependency);
		}
	}

	return writer.data;
}


CompiledCode unserialize(const(ubyte)[] bytes) {
	auto reader = ByteStreamReader(bytes);
	CompiledCode result;

	with (reader) {
		auto magic = take!uint;
		auto format = take!uint;
		auto opcodes = take!uint;
		result.registerCount = take!uint;

		assert(magic == MagicString);
		assert(format == FormatVersion);
		assert(opcodes == OpCodesVersion);

		while (!eos) {
			auto chunk = take!ubyte;
			final switch (cast(Chunks)chunk) with (Chunks) {
			case Instructions:
				assert(result.instrs.empty);
				auto count = take!uint;
				result.instrs.reserve(count);
				foreach (i; 0..count) {
					auto code0 = take!ulong;
					auto code1 = take!ulong;
					result.instrs ~= Instr(code0, code1);
				}
				break;
			case Constants:
				assert(result.constants.empty);
				auto count = take!uint;
				result.constants.reserve(count);
				foreach (i; 0..count) {
					auto type = cast(ConstantType)take!ubyte;
					auto value = take!string;
					result.constants ~= CompiledCode.Constant(type, value);
				}
				break;
			case InstructionSourceLocations:
				assert(result.locs.empty);
				auto count = take!uint;
				result.locs.reserve(count);
				foreach (i; 0..count) {
					auto source = take!uint;
					auto line = take!uint;
					result.locs ~= SourceLoc(source, line, 0);
				}
				break;
			case SourceNames:
				assert(result.sources.empty);
				auto count = take!uint;
				result.sources.reserve(count);
				foreach (i; 0..count)
					result.sources ~= take!string;
				break;
			case Dependencies:
				assert(result.dependencies.empty);
				auto count = take!uint;
				result.dependencies.reserve(count);
				foreach (i; 0..count)
					result.dependencies ~= take!string;
				break;
			}
		}
	}

	return result;
}


private:

enum Chunks : ubyte {
	Instructions = 0,
	Constants,
	InstructionSourceLocations,
	SourceNames,
	Dependencies,
}


enum : uint { MagicString = 0xBB9D1BC5 };
enum : uint { FormatVersion = 1 };



struct ByteStreamWriter {
	void put(T)(T x) if (isIntegral!T) {
		if (offset_ + T.sizeof > stream_.length)
			stream_.length += 16 * 1024;
		// TODO: always write as little-endian
		*cast(T*)(stream_.ptr + offset_) = x;
		length_ += T.sizeof;
		offset_ += T.sizeof;
	}

	void put(T)(T x) if (is(Unqual!T == string)) {
		if (offset_ + T.sizeof > stream_.length)
			stream_.length += 16 * 1024;

		put!uint(cast(uint)x.length);

		(cast(char*)stream_.ptr)[offset_..offset_ + x.length] = x[];
		length_ += x.length;
		offset_ += x.length;
	}

	@property size_t length() const {
		return length_;
	}

	@property const(ubyte)[] data() const {
		return stream_[0..length_];
	}

	private ubyte[] stream_;
	private size_t length_;
	private size_t offset_;
}


struct ByteStreamReader {
	this(const(ubyte)[] stream) {
		stream_ = stream;
	}

	@property size_t length() const {
		return stream_.length;
	}

	@property bool eos() const {
		return offset_ == stream_.length;
	}

	T take(T)() if (isIntegral!T) {
		if (offset_ + T.sizeof > stream_.length)
			throw new Exception("reading past end of stream");
		// TODO: always read as little-endian
		scope(exit) offset_ += T.sizeof;
		return *cast(T*)(stream_.ptr + offset_);
	}

	T take(T)() if (is(Unqual!T == string)) {
		if (offset_ + uint.sizeof > stream_.length)
			throw new Exception("reading past end of stream");

		auto length = take!uint;
		auto result = ((cast(char*)stream_.ptr)[offset_..offset_ + length]).idup;
		offset_ += length;
		return result;
	}

	private const(ubyte)[] stream_;
	private size_t offset_;
}
