system startup
shutDown: isImageQuitting
	isImageQuitting ifTrue: [ self flushAndVoidStdioFiles ]