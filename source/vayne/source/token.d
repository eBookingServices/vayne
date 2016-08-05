module vayne.source.token;


import std.array;
import std.ascii;
import std.format;


import vayne.source.source;


struct Token {
	enum Kind : ubyte {
		Undefined = 0,
		EndOfInput,
		Separator,
		Identifier,
		Keyword,
		Literal,
	}

	enum KeywordKind : ubyte {
		False,
		Null,
		True,
		In,
		Def,
		Undef,
		Set,
		Push,
		Pop,
		As,
	}

	enum LiteralKind : ubyte {
		Char = 0,
		String,
		Bin,
		Oct,
		Dec,
		Hex,
		Float,
	}

	enum Flags : ubyte {
		None 			= 0,
		NeedsUnescaping   = 1 << 0,
	}

	this(SourceLoc loc) {
		kind_ = Kind.Undefined;
		loc_ = loc;
	}

	this(string name, SourceLoc loc) {
		name_ = name;
		kind_ = Kind.Identifier;
		loc_ = loc;
	}

	this(Kind kind, string name, SourceLoc loc) {
		name_ = name;
		kind_ = kind;
		loc_ = loc;
	}

	this(string name, LiteralKind kindLiteral, size_t suffixSize, size_t flags, SourceLoc loc) {
		assert(flags < typeof(flags).max);
		assert(suffixSize < typeof(suffixSize).max);

		kind_ = Kind.Literal;
		kindLiteralOrKeyword_ = kindLiteral;
		literalSuffixSize_ = cast(ubyte)suffixSize;
		flags_ = cast(ubyte)flags;
		name_ = name;
		loc_ = loc;
	}

	this(string name, KeywordKind kindKeyword, size_t flags, SourceLoc loc) {
		assert(flags < typeof(flags).max);

		kind_ = Kind.Keyword;
		kindLiteralOrKeyword_ = kindKeyword;
		literalSuffixSize_ = 0;
		flags_ = cast(ubyte)flags;
		name_ = name;
		loc_ = loc;
	}

	auto opEquals(in Token other) const {
		return (bits_ == other.bits_) && (name_ == other.name_);
	}

	auto eoi() const {
		return kind_ == Kind.EndOfInput;
	}

	auto ident(string name = null) const {
		return isa(Kind.Identifier, name);
	}

	auto sep(string name = null) const {
		return isa(Kind.Separator, name);
	}

	auto sep(char ch) const {
		return (name_.length == 1) && (name_.front == ch);
	}

	auto literal(string name = null) const {
		return isa(Kind.Literal, name);
	}

	auto keyword() const {
		return (kind_ == Kind.Keyword);
	}

	auto keyword(KeywordKind keyword) const {
		return (kind_ == Kind.Keyword) && (kindLiteralOrKeyword_ == keyword);
	}

	auto numeric() const {
		return (kind_ == Kind.Literal) && (kindLiteralOrKeyword_ > LiteralKind.String);
	}

	auto isa(Kind kind, string name = null) const {
		return (kind_ == kind) && (name.empty || (name == name_));
	}

	SourceLoc loc() const {
		return loc_;
	}

	auto needsUnescaping() const {
		assert(kind_ == Kind.Literal);
		return (flags_ & Flags.NeedsUnescaping) != 0;
	}

	string value() const {
		return name_;
	}

	string name() const {
		final switch (kind_) with (Kind) {
		case Literal:
			return "literal";
		case Identifier:
		case Separator:
		case Keyword:
			return name_;
		case EndOfInput:
			return "end of source";
		}
	}

	@property auto ptr() const {
		return name_.ptr;
	}

	@property auto length() const {
		return name_.length;
	}

	@property auto empty() const {
		return name_.empty;
	}

	@property auto tail() const {
		return name_.ptr + name_.length;
	}

	@property auto front() const {
		return name_.front;
	}

	@property auto back() const {
		return name_.back;
	}

	@property auto opIndex(size_t index) const {
		return name_[index];
	}

	@property KeywordKind kindKeyword() const {
		assert(kind_ == Kind.Keyword);
		return cast(KeywordKind)(kindLiteralOrKeyword_);
	}

	@property LiteralKind kindLiteral() const {
		assert(kind_ == Kind.Literal);
		return cast(LiteralKind)(kindLiteralOrKeyword_);
	}

	@property string suffix() const {
		assert(kind_ == Kind.Literal);
		return tail[0..literalSuffixSize_];
	}

	@property Kind kind() const {
		return cast(Kind)(kind_);
	}

	@property auto unescaped() const {
		static int hexValue(char x) {
			assert(isHexDigit(x));
			return (x <= '9') ? (x - '0') : (10 + (toLower(x) - 'a'));
		}

		assert(kind == Kind.Literal);

		if (!needsUnescaping)
			return value;

		auto ptr = name_.ptr;
		auto end = ptr + name_.length;

		auto app = appender!string;

		auto run = ptr;
		while (ptr != end) {
			auto ch = *ptr++;
			if (ch != '\\')
				continue;

			app.put(run[0..(ptr - run) - 1]);

			switch (*ptr) {
			case 'n': app.put('\n'); ++ptr; break;
			case 'r': app.put('\r'); ++ptr; break;
			case 't': app.put('\t'); ++ptr; break;
			case '\\':app.put('\\'); ++ptr; break;
			case '\"':app.put('"');  ++ptr; break;
			case '\'':app.put('\''); ++ptr; break;
			case '?': app.put(cast(char)0x3f); ++ptr; break;
			case 'a': app.put('\a'); ++ptr; break;
			case 'b': app.put('\b'); ++ptr; break;
			case 'f': app.put('\f'); ++ptr; break;
			case 'v': app.put('\v'); ++ptr; break;
			case 'x':
				++ptr;
				if (isHexDigit(*ptr)) {
					int code = hexValue(*ptr);
					++ptr;
					if (isHexDigit(*ptr)) {
						code <<= 4;
						code |= hexValue(*ptr);
						++ptr;
					}
					app.put(cast(dchar)code);
				} else {
					assert(0); // invalid escape sequence - espected digit after \x
				}
				break;
			default:
				if (isOctalDigit(*ptr)) {
					int code = *ptr - '0';
					++ptr;
					if (isOctalDigit(*ptr)) {
						code <<= 3;
						code |= *ptr - '0';
						++ptr;
						if (isOctalDigit(*ptr)) {
							code <<= 3;
							code |= *ptr - '0';
							++ptr;
						}
						app.put(cast(dchar)code);
					}
				} else {
					assert(0); // invalid escape sequence
				}
				break;
			}
			run = ptr;
		}

		app.put(run[0.. end - run]);
		return app.data;
	}

	string toString() const {
		if ((kind == Kind.Literal) && (kindLiteral == LiteralKind.String))
			return format("\"%s\"", name_);
		return name_;
	}

private:
	union {
		uint bits_;
		struct {
			ubyte kind_;
			ubyte kindLiteralOrKeyword_;
			ubyte literalSuffixSize_;
			ubyte flags_;
		}
	}

	string name_;
	SourceLoc loc_;
}
