fixed blocks
fixedBlockSizeFor: lTree and: dTree
	"Compute the length for the current block using fixed huffman trees"
	| bits extra |
	bits := 3 "block type".
	"Compute the size of the compressed block"
	0 to: NumLiterals do:[:i| "encoding of literals"
		bits := bits + ((literalFreq at: i+1) * (FixedLiteralTree bitLengthAt: i))].
	NumLiterals+1 to: lTree maxCode+1 do:[:i| "Encoding of match lengths"
		extra := ExtraLengthBits at: i-NumLiterals.
		bits := bits + ((literalFreq at: i+1) * ((FixedLiteralTree bitLengthAt: i) + extra))].
	0 to: dTree maxCode do:[:i| "encoding of distances"
		extra := ExtraDistanceBits at: i+1.
		bits := bits + ((distanceFreq at: i+1) * ((FixedDistanceTree bitLengthAt: i) + extra))].

	^bits