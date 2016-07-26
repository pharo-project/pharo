*Manifest-Tests
testManifestReadOn
	| method |
	method := thisContext method.

 	self
		assert: (RGMethodDefinition manifestReadOn: {
			self class name asSymbol .
			method selector .
			#false })
		equals: method asRingDefinition