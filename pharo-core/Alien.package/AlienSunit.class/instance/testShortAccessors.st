testing
testShortAccessors
	| alien |
	alien := Alien newC: 2. 
	alien signedShortAt: 1 put: 16r7FFF.
	self should: [(alien signedShortAt: 1) = 16r7FFF].
	self should: [(alien asSignedShort) = 16r7FFF].
	self should: [alien signedShortAt: 1 put: 16r8000] raise: Error.
	alien signedShortAt: 1 put: -1*16r8000.
	self should: [(alien signedShortAt: 1) =  (-1*16r8000)].
	self should: [alien signedShortAt: 1 put: -1*16r8001] raise: Error.
	alien unsignedShortAt: 1 put: 16r7FFF.
	self should: [(alien signedShortAt: 1) = 16r7FFF].
	alien unsignedShortAt: 1 put: 16r8000.
	self should: [(alien signedShortAt: 1) = -32768].
	alien unsignedShortAt: 1 put: 16r8001.
	self should: [(alien signedShortAt: 1) = -32767].
	alien unsignedShortAt: 1 put: 16r7FFF.
	self should: [(alien unsignedShortAt: 1) = 16r7FFF].
	self should: [(alien asUnsignedShort) = 16r7FFF].
	alien unsignedShortAt: 1 put: 16r8000.
	self should: [(alien unsignedShortAt: 1) = 16r8000].
	alien unsignedShortAt: 1 put: 16r8001.
	self should: [(alien unsignedShortAt: 1) = 16r8001].
		alien free.