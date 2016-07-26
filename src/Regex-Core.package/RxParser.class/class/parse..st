utilities
parse: aString
	"Parse the argument and return the result (the parse tree).
	In case of a syntax error, the corresponding exception is signaled."

	^self new parse: aString