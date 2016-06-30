module vayne.source.compress;


import std.array;
import std.regex;
import std.string;


enum CompressOptions : uint {
	none					= 0,
	removeMultiSpaces 		= 1 << 0,	// replace multiple consecutive spaces with a single space
	removeLineBreaks		= 1 << 1,	// replace line breaks with a space
	removeHTMLComments 		= 1 << 2,	// remove any html comments - preserving conditional comments
	removeTagSpaces			= 1 << 3,	// remove unnecessary spaces around = in tags i.e. <div id = "foo"> -> <div id="foo">
	removeTagQuotes			= 1 << 4,	// remove unnecessary quotes around attribute values <div id="foo"> -> <div id=foo>
	removeTagSurroundSpaces	= 1 << 5,	// remove spaces around some tags i.e <ul> <li>

	defaults = ~0U,
}


auto compress(string content, CompressOptions options) {
	if ((options & CompressOptions.removeLineBreaks) == 0)
		content = content.replaceAll(linebreaks, "%%~LB~%%");

	if (options & CompressOptions.removeMultiSpaces)
		content = content.replaceAll(multispaces, " ");

	if (options & CompressOptions.removeHTMLComments)
		content = content.replaceAll(htmlComments, "");

	if (options & CompressOptions.removeTagSpaces) {
		content = content.replaceAll(tagSpaces, "$1=");

		static string removeEndSpaces(Captures!(string) capture) {
			// keep space if attribute is unquoted before trailing slash
			return ((capture[2][0] == '/') && (!matchAll(capture[1], tagSpacesEndLastQuote).empty)) ? (capture[1] ~ " " ~ capture[2]) : (capture[1] ~ capture[2]);
		}

		content = content.replaceAll!removeEndSpaces(tagSpacesEnd);
	}

	if (options & CompressOptions.removeTagQuotes) {
		static string removeQuotes(Captures!(string) capture) {
			return (capture[3].strip.empty) ? ("=" ~ capture[2]) : format("=%s %s", capture[2], capture[3]);
		}

		content = content.replaceAll!removeQuotes(tagQuotes);
	}

	if (options & CompressOptions.removeTagSurroundSpaces)
		content = content.replaceAll(tagSurround, "$1");

	if ((options & CompressOptions.removeLineBreaks) == 0)
		content = content.replace("%%~LB~%%", "\n");

	return content;
}


private __gshared {
	auto multispaces = regex(`\s+`, "i");
	auto linebreaks = regex(`(?:\r\n)|(?:\n)`, "i");
	auto htmlComments = regex(`<!---->|<!--[^\[].*?-->`, "i");
	auto tagSpaces = regex(`(\s\w+)\s*=\s*(?=[^<]*?>)`, "i");
	auto tagSpacesEnd = regex(`(<\w+(?:\s+[a-z0-9-_]+(?:\s*=\s*(?:(?:[a-z0-9-_]+)|(?:"[^"]*")|(?:'[^']*')))?)*)(?:\s+?)(/?>)`, "i");
	auto tagSpacesEndLastQuote = regex(`"=\s*[a-z0-9-_]+$"`, "i");
	auto tagQuotes = regex(`\s*=\s*(["'])([a-z0-9-_]+?)\1(/?)(?=[^<]*?>)`, "i");
	auto tagSurround = regex(`\s*(</?(?:html|head|link|script|style|body|br|p|div|center|dl|form|hr|ol|ul|table|tbody|tr|td|th|tfoot|thead)(?:>|[\s/][^>]*>))\s*`, "i");
	auto tagInterSpace = regex(`>\s+<`, "i");
}
