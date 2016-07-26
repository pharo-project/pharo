dynamic blocks
dynamicBlockSizeFor: lTree and: dTree using: blTree and: blFreq
	"Compute the length for the current block using dynamic huffman trees"
	| bits index extra treeBits freq |
	bits := 3 "block type" + 5 "literal codes length" + 5 "distance codes length".

	"Compute the # of bits for sending the bit length tree"
	treeBits := 4. "Max index for bit length tree"
	index := MaxBitLengthCodes.
	[index >= 4] whileTrue:[
		(index = 4 or:[(blFreq at: (BitLengthOrder at: index)+1) > 0])
			ifTrue:[treeBits := treeBits + (index * 3).
					index := -1]
			ifFalse:[index := index - 1]].

	"Compute the # of bits for sending the literal/distance tree.
	Note: The frequency are already stored in the blTree"
	0 to: 15 do:[:i| "First, the non-repeating values"
		freq := blFreq at: i+1.
		freq > 0 ifTrue:[treeBits := treeBits + (freq * (blTree bitLengthAt: i))]].
	"Now the repeating values"
	(Repeat3To6 to: Repeat11To138) with: #(2 3 7) do:[:i :addl|
		freq := blFreq at: i+1.
		freq > 0 ifTrue:[
			treeBits := treeBits + (freq * ((blTree bitLengthAt: i) + addl "addl bits"))]].
	VerboseLevel > 1 ifTrue:[
		Transcript show:'['; print: treeBits; show:' bits for dynamic tree]'].
	bits := bits + treeBits.

	"Compute the size of the compressed block"
	0 to: NumLiterals do:[:i| "encoding of literals"
		freq := literalFreq at: i+1.
		freq > 0 ifTrue:[bits := bits + (freq * (lTree bitLengthAt: i))]].
	NumLiterals+1 to: lTree maxCode do:[:i| "encoding of match lengths"
		freq := literalFreq at: i+1.
		extra := ExtraLengthBits at: i-NumLiterals.
		freq > 0 ifTrue:[bits := bits + (freq * ((lTree bitLengthAt: i) + extra))]].
	0 to: dTree maxCode do:[:i| "encoding of distances"
		freq := distanceFreq at: i+1.
		extra := ExtraDistanceBits at: i+1.
		freq > 0 ifTrue:[bits := bits + (freq * ((dTree bitLengthAt: i) + extra))]].

	^bits