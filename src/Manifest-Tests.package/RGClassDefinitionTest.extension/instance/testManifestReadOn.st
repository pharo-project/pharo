*Manifest-Tests
testManifestReadOn
	| class |
	class := self class.

 	self
		assert: (RGClassDefinition manifestReadOn: { class name asSymbol })
		equals: class asRingDefinition 