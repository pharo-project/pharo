visitor-double dispatching
visitLargeArrayNode: aRBArrayNode 
	"Long form: generates (Array braceStream: N) nextPut: a; nextPut: b; ...; braceArray"
	methodBuilder pushLiteralVariable: Array binding.
	methodBuilder pushLiteral: aRBArrayNode statements size.
	methodBuilder send: #braceStream:.
	aRBArrayNode statements do: [ :each |
		methodBuilder pushDup.
		valueTranslator visitNode: each.
		methodBuilder send: #nextPut:.
		methodBuilder popTop.
		].
	methodBuilder send: #braceArray.
				