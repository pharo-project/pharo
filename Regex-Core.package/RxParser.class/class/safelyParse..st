utilities
safelyParse: aString
	"Parse the argument and return the result (the parse tree).
	In case of a syntax error, return nil.
	Exception handling here is dialect-dependent."
	^ [self new parse: aString] on: RegexSyntaxError do: [:ex | nil]