accessing
compressSourceCode: aString
	"Embed the source code into compiled method trailer, 
	pick best compression method"
	| temp |
	self clear.
	kind := #EmbeddedSourceQCompress.
	data := aString asString. "add Text support in future?"
	
	self encode.
	temp := encodedData.

	kind := #EmbeddedSourceZip.
	self encode.
	encodedData size > temp size ifTrue: [
		encodedData := temp.
		kind := #EmbeddedSourceQCompress.
		size := encodedData size.
		]