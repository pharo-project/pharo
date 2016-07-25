util
compileMethod: aMethod

	^aMethod parseTree generate: aMethod trailer.
	