printing
formattedText
	"Answer a string or text representing the receiver with indentation and, possibly, markup."

	|str|
	str := String new writeStream.
	self putFormattedTextOn: str level: 0 indentString: '  '.
	^str contents