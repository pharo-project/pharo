*Zinc-Character-Encoding-Core
utf8Encoded
	"Produce a ByteArray encoding the receiver using UTF-8,
	the recommended encoding for Strings, unless you know what you are doing."
	
	" 'Les élèves français' utf8Encoded "
	
	^ self encodeWith: ZnCharacterEncoder utf8