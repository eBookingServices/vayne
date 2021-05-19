module vayne.hash;


hash_t hashOf(T)(auto ref T x, size_t seed = 0) if (is(T == enum)) {
    return hashOf(cast(EType)x, seed);
}


hash_t hashOf(T)(auto ref T x, size_t seed = 0) if (!is(T == enum) && __traits(isStaticArray, T)) {
    auto hash = seed;
    foreach (ref e; x)
        hash = hashOf(e, hash);
    return hash;
}


hash_t hashOf(T)(auto ref T x, size_t seed = 0) if (!is(T == enum) && !is(T : typeof(null)) && is(T S: S[]) && !__traits(isStaticArray, T) && !is(T == struct) && !is(T == class) && !is(T == union)) {
    alias ElementType = typeof(x[0]);

    static if (is(ElementType == interface) || is(ElementType == class) || ((is(ElementType == struct) || is(ElementType == union)) && is(typeof(x[0].toHash()) == size_t))) {
        auto hash = seed;
        foreach (o; x)
            hash = hashOf(o, hash);
        return hash;
    } else {
        return hash(x.ptr, ElementType.sizeof * x.length, seed);
    }
}


@trusted nothrow pure
hash_t hashOf(T)(auto ref T x, size_t seed = 0) if (!is(T == enum) && __traits(isArithmetic, T)) {
    static if(__traits(isFloating, x)) {
        T y = (x != x) ? T.nan : x;
        return hash(cast(ubyte*)&y, T.sizeof, seed);
    } else {
		return hash(cast(ubyte*)&x, T.sizeof, seed);
    }
}


@trusted nothrow pure
hash_t hashOf(T)(auto ref T x, size_t seed = 0) if (!is(T == enum) && is(T : typeof(null))) {
    return hashOf(cast(void*)null);
}


@trusted nothrow pure
hash_t hashOf(T)(auto ref T x, size_t seed = 0) if (!is(T == enum) && is(T V : V*) && !is(T : typeof(null)) && !is(T == struct) && !is(T == class) && !is(T == union)) {
    return hashOf(cast(size_t)x);
}


hash_t hashOf(T)(auto ref T x, size_t seed = 0) if (!is(T == enum) && (is(T == struct) || is(T == union))) {
    static if (is(typeof(x.toHash()) == size_t)) {
        return hashOf(x.toHash(), seed);
    } else {
        return hash(cast(ubyte*)&x, T.sizeof, seed);
    }
}


@trusted nothrow pure
hash_t hashOf(T)(auto ref T x, size_t seed = 0) if (!is(T == enum) && is(T == delegate)) {
    return hash(cast(ubyte*)&x, T.sizeof, seed);
}


hash_t hashOf(T)(auto ref T x, size_t seed = 0) if (!is(T == enum) && is(T == interface) || is(T == class)) {
    return hashOf(x ? (cast(Object)x).toHash() : 0, seed);
}


hash_t hashOf(T)(auto ref T x, size_t seed = 0) if (!is(T == enum) && __traits(isAssociativeArray, T)) {
	if (!x.length)
		return hashOf(0, seed);

    size_t h = 0;
    foreach (k, ref v; x) {
        size_t[2] hpair;
        hpair[0] = k.hashOf();
        hpair[1] = v.hashOf();
        h ^= hpair.hashOf();
    }
    return h.hashOf(seed);
}


@trusted nothrow pure
hash_t hash(T)(in const(T)* x, size_t len, size_t seed = 0) {
	version(X86_64) {
		return hash64(x, len, seed);
	} else version(AArch64) {
		return hash64(x, len, seed);
	} else {
		return hash32(x, len, seed);
	}
}


@trusted nothrow pure
uint hash32(T)(in const(T)* x, size_t len, uint seed = 0) if (__traits(isArithmetic, T) && (T.sizeof == 1)) {
	uint h = cast(uint)len;
	const(ubyte)* p = cast(const(ubyte)*)x;
	const(ubyte)* end = p + len;

	if (len >= 16) {
		const const(ubyte)* limit = p + (len - 16);
		uint v1 = seed + Prime32_1 + Prime32_2;
		uint v2 = seed + Prime32_2;
		uint v3 = seed + 0;
		uint v4 = seed - Prime32_1;

		do {
			v1 += read32(p) * Prime32_2; v1 = rotl32!13(v1); v1 *= Prime32_1; p += 4;
			v2 += read32(p) * Prime32_2; v2 = rotl32!13(v2); v2 *= Prime32_1; p += 4;
			v3 += read32(p) * Prime32_2; v3 = rotl32!13(v3); v3 *= Prime32_1; p += 4;
			v4 += read32(p) * Prime32_2; v4 = rotl32!13(v4); v4 *= Prime32_1; p += 4;
		} while (p <= limit);

		h += rotl32!1(v1) + rotl32!7(v2) + rotl32!12(v3) + rotl32!18(v4);
	} else {
		h += seed + Prime32_5;
	}

	while ((p + 4) <= end) {
		h += read32(p) * Prime32_3;
		h = rotl32!17(h) * Prime32_4;
		p += 4;
	}

	while (p != end) {
		h += (*p) * Prime32_5;
		h = rotl32!11(h) * Prime32_1;
		++p;
	}

	h ^= h >> 15;
	h *= Prime32_2;
	h ^= h >> 13;
	h *= Prime32_3;
	h ^= h >> 16;
	return h;
}


@trusted nothrow pure
ulong hash64(T)(in const(T)* x, size_t len, ulong seed = 0) if (__traits(isArithmetic, T) && (T.sizeof == 1)) {
	ulong h = len;
	const(ubyte)* p = cast(const(ubyte)*)x;
	const(ubyte)* end = p + len;

	if (len >= 32) {
		const const(ubyte)* limit = p + (len - 32);
		ulong v1 = seed + Prime64_1 + Prime64_2;
		ulong v2 = seed + Prime64_2;
		ulong v3 = seed + 0;
		ulong v4 = seed - Prime64_1;

		do {
			v1 += read64(p) * Prime64_2; v1 = rotl64!31(v1); v1 *= Prime64_1; p += 8;
			v2 += read64(p) * Prime64_2; v2 = rotl64!31(v2); v2 *= Prime64_1; p += 8;
			v3 += read64(p) * Prime64_2; v3 = rotl64!31(v3); v3 *= Prime64_1; p += 8;
			v4 += read64(p) * Prime64_2; v4 = rotl64!31(v4); v4 *= Prime64_1; p += 8;
		} while (p <= limit);

		h += rotl64!1(v1) + rotl64!7(v2) + rotl64!12(v3) + rotl64!18(v4);

		v1 *= Prime64_2; v1 = rotl64!31(v1); v1 *= Prime64_1; h ^= v1;
		h = h * Prime64_1 + Prime64_4;

		v2 *= Prime64_2; v2 = rotl64!31(v2); v2 *= Prime64_1; h ^= v2;
		h = h * Prime64_1 + Prime64_4;

		v3 *= Prime64_2; v3 = rotl64!31(v3); v3 *= Prime64_1; h ^= v3;
		h = h * Prime64_1 + Prime64_4;

		v4 *= Prime64_2; v4 = rotl64!31(v4); v4 *= Prime64_1; h ^= v4;
		h = h * Prime64_1 + Prime64_4;
	} else {
		h += seed + Prime64_5;
	}

	while ((p + 8) <= end) {
		ulong k = read64(p);
		k *= Prime64_2; k = rotl64!31(k); k *= Prime64_1; h ^= k;
		h = rotl64!27(h) * Prime64_1 + Prime64_4;
		p += 8;
	}

	while ((p + 4) <= end) {
		h ^= read32(p) * Prime64_1;
		h = rotl64!23(h) * Prime32_2 + Prime64_3;
		p += 4;
	}

	while (p != end) {
		h ^= (*p) * Prime64_5;
		h = rotl64!11(h) * Prime32_1;
		++p;
	}

	h ^= h >> 33;
	h *= Prime64_2;
	h ^= h >> 29;
	h *= Prime64_3;
	h ^= h >> 32;
	return h;
}


private {
	enum : uint {
		Prime32_1 = 2654435761U,
		Prime32_2 = 2246822519U,
		Prime32_3 = 3266489917U,
		Prime32_4 = 668265263U,
		Prime32_5 = 374761393U,
	}

	uint rotl32(size_t K)(in uint x) nothrow pure {
		return (x << K) | (x >> (32 - K));
	}

	uint read32(const ubyte* p) nothrow pure {
		return *cast(uint*)p;
	}

	enum : ulong {
		Prime64_1 = 11400714785074694791UL,
		Prime64_2 = 14029467366897019727UL,
		Prime64_3 = 1609587929392839161UL,
		Prime64_4 = 9650029242287828579UL,
		Prime64_5 = 2870177450012600261UL,
	}

	ulong rotl64(size_t K)(in ulong x) nothrow pure {
		return (cast(ulong)x << K) | (cast(ulong)x >> (64 - K));
	}

	ulong read64(const ubyte* p) nothrow pure {
		return *cast(ulong*)p;
	}
}
