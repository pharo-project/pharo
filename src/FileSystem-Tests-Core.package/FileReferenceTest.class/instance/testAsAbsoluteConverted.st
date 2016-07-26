tests
testAsAbsoluteConverted
	"Converts a relative reference to absolute, and asserts
	that it's absolute and still has the same path."
	
	| ref absolute |
	ref := filesystem * 'plonk'.
	absolute := ref asAbsolute.
	self assert: absolute isAbsolute.
	self assert: (absolute path at: 1) = 'plonk'