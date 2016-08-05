module vayne.source.lexer;


import std.array;
import std.ascii;
import std.exception;
import std.format;
import std.string;


import vayne.source.source;
import vayne.source.token;



class LexerException : Exception {
	this(T)(T* ptr, string msg) {
		super(msg);
		this.ptr = cast(void*)ptr;
	}

	void* ptr;
}


struct Lexer {
	this(Source source, SourceLoc loc) {
		source_ = source;
		empty_ = false;
		loc_ = loc;

		popFront;
	}

	this(Source source) {
		this(source, SourceLoc(source.id, 1));
	}

	void popFront() {
		assert(!empty);

		if (current_.kind == Token.Kind.EndOfInput) {
			empty_ = true;
		} else {
			eat;
		}
	}

	Token front() const {
		return current_;
	}

	bool empty() const {
		return empty_;
	}

private:
	void eat() {
		auto ptr = source_.buffer.ptr;
		scope (exit) source_.ptr = ptr;
		auto end = source_.end;

		eatSpaces(ptr, end);

		while (ptr != end) {
			auto start = ptr;
			auto ch = *ptr++;

			switch(ch) {
			case '\"':
			case '\'':
			case '`':
				current_ = eatString(ch, ptr, end);
				return;
			case '/':
				if (ptr != end) {
					if (*ptr == ch) {
						eatUntil('\n', ptr, end);
						eatSpaces(ptr, end);
						continue;
					} else if (*ptr == '*') {
						eatBlockComment(ptr, end);
						eatSpaces(ptr, end);
						continue;
					}
				}
				current_ = eatSep(ch, ptr - 1, ptr, end);
				return;
			case '.':
				if (ptr != end) {
					if (*ptr == '.') {
						++ptr;
						if ((ptr != end) && (*ptr == '.'))
							++ptr;
					} else if (isDigit(*ptr)) {
						current_ = eatNumeric(ch, ptr - 1, ptr, end);
						return;
					}
				}
				current_ = Token(Token.Kind.Separator, start[0..ptr - start], loc_);
				return;
			default:
				if (isAlpha(ch) || (ch == '_')) {
					while ((ptr != end) && (isAlphaNum(*ptr) || (*ptr == '_')))
						++ptr;
					auto length = ptr - start;
					auto name = start[0..length];

					switch(length) {
					case 2:
						if (name == "in") {
							current_ = Token(name, Token.KeywordKind.In, 0, loc_);
							return;
						}
						if (name == "as") {
							current_ = Token(name, Token.KeywordKind.As, 0, loc_);
							return;
						}
						break;
					case 3:
						if (name == "def") {
							current_ = Token(name, Token.KeywordKind.Def, 0, loc_);
							return;
						}
						if (name == "set") {
							current_ = Token(name, Token.KeywordKind.Set, 0, loc_);
							return;
						}
						if (name == "pop") {
							current_ = Token(name, Token.KeywordKind.Pop, 0, loc_);
							return;
						}
						break;
					case 4:
						if (name == "null") {
							current_ = Token(name, Token.KeywordKind.Null, 0, loc_);
							return;
						}
						if (name == "true") {
							current_ = Token(name, Token.KeywordKind.True, 0, loc_);
							return;
						}
						if (name == "push") {
							current_ = Token(name, Token.KeywordKind.Push, 0, loc_);
							return;
						}
						break;
					case 5:
						if (name == "false") {
							current_ = Token(name, Token.KeywordKind.False, 0, loc_);
							return;
						}
						if (name == "undef") {
							current_ = Token(name, Token.KeywordKind.Undef, 0, loc_);
							return;
						}
						break;
					default:
						break;
					}
					current_ = Token(Token.Kind.Identifier, start[0..length], loc_);
				} else if (isDigit(ch)) {
					current_ = eatNumeric(ch, ptr - 1, ptr, end);
				} else {
					current_ = eatSep(ch, ptr - 1, ptr, end);
				}
				return;
			}
		}

		if (ptr >= end)
			current_ = Token(Token.Kind.EndOfInput, null, loc_);
	}

	void eatSpaces(T)(ref T* ptr, ref T* end) {
		while ((ptr != end) && isWhite(*ptr)) {
			if (*ptr == '\n')
				++loc_.line;
			++ptr;
		}
	}

	void eatUntil(T)(T ch, ref T* ptr, T* end) {
		while ((ptr != end) && (*ptr != ch)) {
			if (*ptr == '\n')
				++loc_.line;
			++ptr;
		}
	}

	void eatBlockComment(T)(ref T* ptr, T* end) {
		auto start = ptr;
		auto opens = 1;

		while (opens) {
			if (ptr == end)
				throw new LexerException(start, "unterminated block comment");

			while((ptr != end) && (*ptr != '/')) {
				if (*ptr == '\n')
					++loc_.line;
				++ptr;
			}

			if (*ptr == '/') {
				if (*(ptr - 1) == '*') {
					++ptr;
					--opens;
				} else {
					++ptr;
					if ((ptr != end) && (*ptr == '*')) {
						++ptr;
						++opens;
					}
				}
			}
		}
	}

	Token eatSep(T)(T ch, T* start, ref T* ptr, T* end) {
		assert(!isWhite(ch));

		auto doubleEnabled = false;
		auto equalsEnabled = false;
		auto doubleEquals = false;

		if (ptr != end) {
			switch(ch) {
			case '>':
			case '<':
			case '^':
				doubleEquals = true;
				doubleEnabled = true;
				equalsEnabled = true;
				goto default;
			case '+':
			case '-':
			case '|':
			case '&':
				doubleEnabled = true;
				equalsEnabled = true;
				goto default;
			case '*':
			case '/':
			case '%':
			case '!':
			case '~':
			case '=':
				equalsEnabled = true;
				goto default;
			default:
				if (doubleEnabled) {
					if (*ptr == ch) {
						++ptr;
						if (doubleEquals && (ptr != end)) {
							assert(doubleEnabled);
							if (*ptr == '=')
								++ptr;
						}
					} else if (equalsEnabled && (*ptr == '=')) {
						++ptr;
					}
				} else if (equalsEnabled) {
					if (*ptr == '=')
						++ptr;
				}
			}
		}

		return Token(Token.Kind.Separator, start[0..ptr - start], loc_);
	}

	size_t eatSuffix(T)(ref T* ptr, T* end) {
		auto start = ptr;

		if (ptr != end) {
			if (isAlpha(*ptr) || (*ptr == '_')) {
				++ptr;
				while ((ptr != end) && (isAlphaNum(*ptr) || (*ptr == '_')))
					++ptr;
			}
		}

		return ptr - start;
	}

	Token eatFloat(T)(T ch, T* start, ref T* ptr, T* end) {
		auto dot = (ch == '.');

		while ((ptr != end) && ((isDigit(*ptr) || (*ptr == '\'') || (!dot && (*ptr == '.') && (*(ptr + 1) != '.'))))) {
			if (*ptr == '.')
				dot = true;
			++ptr;
		}

		size_t suffixSize;
		if (ptr != end) {
			if ((*ptr == 'e') || (*ptr == 'E')) {
				++ptr;
				if (ptr != end) {
					if ((*ptr == '-') || (*ptr == '+'))
						++ptr;
					while ((ptr != end) && (isDigit(*ptr)))
						++ptr;
				}
				if (!isDigit(*(ptr - 1)))
					throw new LexerException(ptr - 1, "invalid exponent in floating-point literal");
			}

			suffixSize = eatSuffix(ptr, end);
		}

		return Token(start[0..((ptr - start) - suffixSize)], (dot ? Token.LiteralKind.Float : Token.LiteralKind.Dec), suffixSize, 0, loc_);
	}

	Token eatNumeric(T)(T ch, T* start, ref T* ptr, T* end) {
		if (ch == '0') {
			auto base = Token.LiteralKind.Dec;

			size_t suffixSize;
			if (ptr != end) {
				switch (std.ascii.toLower(*ptr)) {
				case 'x':
					if (isHexDigit(*(ptr + 1))) {
						++ptr;
						base = Token.LiteralKind.Hex;
						while ((ptr != end) && (isHexDigit(*ptr)))
							++ptr;
					}
					break;
				case 'b':
					if ((*(ptr + 1) == '0') || (*(ptr + 1) == '1')) {
						++ptr;
						while ((ptr != end) && ((*ptr == '0') || (*ptr == '1')))
							++ptr;
						base = Token.LiteralKind.Bin;
					}
					break;
				case 'o':
					if ((*(ptr + 1) >= '0') && (*(ptr + 1) <= '7')) {
						++ptr;
						while ((ptr != end) && (*ptr >= '0') && (*ptr <= '7'))
							++ptr;
						base = Token.LiteralKind.Oct;
					}
					break;
				case '.':
					return eatFloat(ch, start, ptr, end);
				case 'd':
					if ((*(ptr + 1) >= '0') && (*(ptr + 1) <= '9')) {
						start = ptr + 1;
						++ptr;
					}
					goto default;
				default:
					while ((ptr != end) && isDigit(*ptr))
						++ptr;
					break;
				}

				suffixSize = eatSuffix(ptr, end);
			}
			return Token(start[0..((ptr - start) - suffixSize)], base, suffixSize, 0, loc_);
		} else {
			return eatFloat(ch, start, ptr, end);
		}
	}

	Token eatString(immutable(char) ch, ref immutable(char)* ptr, immutable(char)* end) {
		auto start = ptr;
		auto needsUnescaping = false;

		while ((ptr != end) && (*ptr != ch)) {
			if (*ptr != '\\') {
				if (*ptr == '\n')
					++loc_.line;
				++ptr;
			} else {
				++ptr;
				if (ptr != end) {
					needsUnescaping = true;
					++ptr;
				}
			}
		}

		if (*ptr != ch)
			throw new LexerException(start, "unterminated string-literal");

		++ptr;
		size_t flags = 0;
		if (needsUnescaping && (ch != '`'))
			flags |= Token.Flags.NeedsUnescaping;

		auto suffixSize = eatSuffix(ptr, end);

		return Token(start[0..(ptr - start - suffixSize - 1)], (((ch == '\"') || (ch == '`')) ? Token.LiteralKind.String : Token.LiteralKind.Char), suffixSize, flags, loc_);
	}

	bool empty_;
	Token current_;
	Source source_;
	SourceLoc loc_;
}
