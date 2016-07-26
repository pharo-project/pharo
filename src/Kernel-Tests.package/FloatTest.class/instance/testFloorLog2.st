tests - mathematical functions
testFloorLog2
	"Float internal representation of Float being in base 2, we expect (aFloat floorLog: 2) to be exact."
	
	| aBitLess aBitMore |
	aBitMore := 1 + Float epsilon.
	aBitLess := 1 - Float epsilon.
	Float emin + 1 to: Float emax - 1 do: [:exp |
		| exactPowerOfTwo |
		exactPowerOfTwo := 1.0 timesTwoPower: exp.
		self assert: (exactPowerOfTwo floorLog: 2) equals: exp.
		self assert: (exactPowerOfTwo * aBitMore floorLog: 2) equals: exp.
		self assert: (exactPowerOfTwo * aBitLess floorLog: 2) equals: exp - 1].