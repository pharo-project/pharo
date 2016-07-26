accessing
embeddedSourceCode: aString
	"Embed the source code into compiled method trailer"

	self clear.
	kind := #EmbeddedSource.
	data := aString asString.
	self encode.