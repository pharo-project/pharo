*Manifest-Tests
testArrayStringForManifest
	| class |
	class := self class.

 	self
		assert: class asRingDefinition arrayStringForManifest
		equals: { #RGClassDefinition . { class name asSymbol } }