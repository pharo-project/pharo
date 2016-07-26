*Manifest-Tests
testManifestReadOn
	| class |
	class := self class.

 	self
		assert: (RGMetaclassDefinition manifestReadOn: { class class name asSymbol . class name asSymbol })
		equals: class class asRingDefinition 
