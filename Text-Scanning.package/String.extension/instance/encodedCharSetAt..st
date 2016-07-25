*Text-Scanning
encodedCharSetAt: index
	"return the character encoding in place at index; the actual EncodedCharSet, not just a number. A bad index is an Error"
	^EncodedCharSet charsetAt: (self at: index) leadingChar