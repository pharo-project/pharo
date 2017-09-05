evaluating
evaluateDeclarations
	"Evaluates the declarations from the text in the file and answers the last result"

	self parseChunks.
	^ self evaluate