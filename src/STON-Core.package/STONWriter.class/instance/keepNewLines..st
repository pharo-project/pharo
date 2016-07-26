initialize-release
keepNewLines: boolean
	"If true, any newline CR, LF or CRLF inside strings or symbols will not be escaped
	but will instead be converted to the newline convention chosen, see #newLine:
	The default is false, where CR, LF or CRLF will be enscaped unchanged."
	
	keepNewLines := boolean