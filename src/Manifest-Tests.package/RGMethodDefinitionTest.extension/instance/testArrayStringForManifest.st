*Manifest-Tests
testArrayStringForManifest
	| method |
	method := thisContext method.

 	self
		assert: method asRingDefinition arrayStringForManifest
		equals: { #RGMethodDefinition . { self class name asSymbol . method selector . #false } }