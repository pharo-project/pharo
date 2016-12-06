reflectivity
emitPreamble: aNode
	aNode preambles do: [ :pre | valueTranslator visitNode: pre].