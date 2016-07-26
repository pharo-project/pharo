printing
formattedTextWithDescriptions: aKeyedTree
	"Answer a string or text representing the receiver with indentation and, possibly, markup.
	Descriptions of each item are taken from the given tree with
	the same key structure as the receiver."
	
	|str|
	str := String new writeStream.
	self putFormattedTextOn: str withDescriptions: aKeyedTree level: 0 indentString: '  '.
	^str contents