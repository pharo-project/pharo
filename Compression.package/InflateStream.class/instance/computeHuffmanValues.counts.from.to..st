huffman trees
computeHuffmanValues: aCollection counts: counts from: minBits to: maxBits
	"Assign numerical values to all codes.
	Note: The values are stored according to the bit length"
	| offsets values baseOffset codeLength |
	offsets := Array new: maxBits.
	offsets atAllPut: 0.
	baseOffset := 1.
	minBits to: maxBits do:[:bits|
		offsets at: bits put: baseOffset.
		baseOffset := baseOffset + (counts at: bits+1)].
	values := WordArray new: aCollection size.
	1 to: aCollection size do:[:i|
		codeLength := aCollection at: i.
		codeLength > 0 ifTrue:[
			baseOffset := offsets at: codeLength.
			values at: baseOffset put: i-1.
			offsets at: codeLength put: baseOffset + 1]].
	^values