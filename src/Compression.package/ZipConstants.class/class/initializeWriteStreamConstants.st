pool initialization
initializeWriteStreamConstants

	MaxBits := 15.
	MaxBitLengthBits := 7.
	EndBlock := 256.

	StoredBlock := 0.
	FixedBlock := 1.
	DynamicBlock := 2.

	NumLiterals := 256.
	MaxLengthCodes := 29.
	MaxDistCodes := 30.
	MaxBitLengthCodes := 19.
	MaxLiteralCodes := NumLiterals + MaxLengthCodes + 1. "+ End of Block"

	Repeat3To6 := 16. "Repeat previous bit length 3-6 times (2 bits repeat count)"
	Repeat3To10 := 17. "Repeat previous bit length 3-10 times (3 bits repeat count)"
	Repeat11To138 := 18. "Repeat previous bit length 11-138 times (7 bits repeat count)"

	self initializeExtraBits.
	self initializeLengthCodes.
	self initializeDistanceCodes.
	self initializeFixedTrees.
