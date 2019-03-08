module vayne.source.mime;


import std.base64;
import std.path;
import std.range;
import std.string;


string mimeType(string fileName) {
	return fileName.extension.extensionMimeType;
}


string extensionMimeType(string ext) {
	switch(ext.toLower) {
	case ".jpg":
	case ".jpeg":
		return "image/jpeg";
	case ".png":
		return "image/png";
	case ".gif":
		return "image/gif";
	case ".tga":
		return "image/targa";
	case ".tif":
	case ".tiff":
		return "image/tiff";
	case ".webp":
		return "image/webp";
	case ".txt":
		return "text/plain";
	case ".html":
		return "text/html";
	default:
		return "application/octet-stream";
	}
}


string encode(string input) {
	Appender!string mime = appender!string;
	foreach (ref encoded; Base64.encoder(chunks(cast(ubyte[])input, 57))) {
		mime.put(encoded);
		mime.put("\r\n");
	}
	return mime.data;
}
