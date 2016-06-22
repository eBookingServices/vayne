### Vayne - Mustache-like template compiler and run-time interpreter for D

Vayne has been written mainly for use with vibe.d but it can be used in any other way.
It is the successor to jax, a compile-time solution.

Compiles source text/html into a bytecode to be interpreted at run time.

### Features
- HTML compressor
- Parametrized macros
- Good and detailed error reporting
- Supports user defined escaping and translation


#### Options
	o|output-dir - Output directory
	p|time - Display elapsed time at end
	v|verbose - Verbose output
	l|line-numbers - Keep line numbers for error reporting (disables compression)
	d|dep-cache - Dependant-cache directory
	g|dep-gen-only - Only generate dependant-cache, do not re-compile dependants
	c|compress - Compress HTML in between template tags
	j|search - Search path(s) to look for dependency files
	t|token	- Token for token_url interpolation filter. Useful for cache-busting


#### Tags
	{{& fileName }} - Include external file
	{{&& fileName }} - Embed external file as mime-encoded content
	{{* key, value; iterable }} {{key}} {{value}} {{/}} - Iterate any iteratable symbol
	{{? expr }} true case {{: [expr] }} else case {{/}}
	{{@ expr0, expr2 as ident }} - Create symbol scopes or named copy of expressions. For symbol scopes, expression must evalute to an object or associative array.
	{{! comment }} - Comment
	{{ expr }} - Write expression to output with automatic HTML escaping
    {{{ expr }}} - Write expression to output without escaping
	{{~ message-id-expr, arg0+ }} - Translate message-id with arguments and output - use triple brackets to disable escaping
	{{#def myMacro(arg) }} macro text {{#arg}} {{#/}}
	{{#myMacro("mooo") }} - Expand a macro


Check the example directory for a working example.
