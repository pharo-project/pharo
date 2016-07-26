pool initialization
initializeFixedTrees
	"ZipWriteStream initializeFixedTrees"
	| counts nodes |
	FixedLiteralTree := ZipEncoderTree new.
	FixedLiteralTree maxCode: 287.
	counts := WordArray new: MaxBits+1.
	counts at: 7+1 put: 24.
	counts at: 8+1 put: 144+8.
	counts at: 9+1 put: 112.
	nodes := Array new: 288.
	1 to: 288 do:[:i| nodes at: i put: (ZipEncoderNode value: i-1 frequency: 0 height: 0)].
	0 to: 143 do:[:i| (nodes at: i+1) setBitLengthTo: 8].
	144 to: 255 do:[:i| (nodes at: i+1) setBitLengthTo: 9].
	256 to: 279 do:[:i| (nodes at: i+1) setBitLengthTo: 7].
	280 to: 287 do:[:i| (nodes at: i+1) setBitLengthTo: 8].
	FixedLiteralTree buildCodes: nodes counts: counts maxDepth: MaxBits.
	FixedLiteralTree setValuesFrom: nodes.

	FixedDistanceTree := ZipEncoderTree new.
	FixedDistanceTree maxCode: MaxDistCodes.
	FixedDistanceTree
		bitLengths: ((WordArray new: MaxDistCodes+1) atAllPut: 5)
		codes: ((0 to: MaxDistCodes) collect:[:i| FixedDistanceTree reverseBits: i length: 5]).