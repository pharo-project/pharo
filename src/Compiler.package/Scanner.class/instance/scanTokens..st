public access
scanTokens: textOrString 
	"Answer an Array that has been tokenized as though the input text, 
	textOrString, had appeared between the array delimitors #( and ) in a 
	Smalltalk literal expression."

	self scan: textOrString asString readStream.
	self scanLitVec.
	^token

	"Scanner new scanTokens: 'identifier keyword: 8r31 ''string'' .'"