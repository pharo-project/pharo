testing
testByteAccessors
	| alien |
	alien := Alien newC: 2. 
	alien signedByteAt: 1 put: 16r7F.
	self should: [(alien signedByteAt: 1) = 16r7F].
	self should: [(alien asSignedByte) = 16r7F].
	self should: [alien signedByteAt: 1 put: 16r80] raise: Error.
	alien signedByteAt: 2 put: 16r00.
	alien signedByteAt: 1 put: 16r7F.
	self should: [(alien signedByteAt: 2) = 16r00].
	alien signedByteAt: 1 put: -1*16r80.
	self should: [(alien signedByteAt: 1) =  (-1*16r80)].
	self should: [alien signedByteAt: 1 put: -1*16r81] raise: Error.
	alien unsignedByteAt: 1 put: 16r7F.
	self should: [(alien signedByteAt: 1) = 16r7F].
	alien unsignedByteAt: 1 put: 16r80.
	self should: [(alien asSignedByte) = -128].
	self should: [(alien signedByteAt: 1) = -128].
	alien unsignedByteAt: 1 put: 16r81.
	self should: [(alien signedByteAt: 1) = -127].
	alien unsignedByteAt: 1 put: 16r7F.
	self should: [(alien unsignedByteAt: 1) = 16r7F].
	alien unsignedByteAt: 1 put: 16r80.
	self should: [(alien unsignedByteAt: 1) = 16r80].
	alien unsignedByteAt: 1 put: 16r81.
	self should: [(alien unsignedByteAt: 1) = 16r81].
	alien free.