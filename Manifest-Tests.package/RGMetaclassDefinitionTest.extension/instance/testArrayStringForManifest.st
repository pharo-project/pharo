*Manifest-Tests
testArrayStringForManifest
	| class |
	class := self class class.

 	self
		assert: class asRingDefinition arrayStringForManifest
		equals: { #RGMetaclassDefinition . { class name asSymbol . self class name asSymbol } }