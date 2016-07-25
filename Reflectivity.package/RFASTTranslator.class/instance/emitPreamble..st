reflectivity
emitPreamble: aNode
	aNode preambles do: [ :pre | self visitNode: pre].