testing
testLongAccessors
	| alien |
	Smalltalk wordSize = 4 ifFalse: [^self].
	
	alien := Alien newC: 4. 
	alien signedLongAt: 1 put: 16r7FFFFFFF.
	self should: [(alien signedLongAt: 1) = 16r7FFFFFFF].
	self should: [(alien asSignedLong) = 16r7FFFFFFF].
	self should: [alien signedLongAt: 1 put: 16r80000000] raise: Error.
	alien signedLongAt: 1 put: -1*16r80000000.
	self should: [(alien signedLongAt: 1) =  (-1*16r80000000)].
	self should: [alien signedLongAt: 1 put: -1*16r80000001] raise: Error.
	alien unsignedLongAt: 1 put: 16r7FFFFFFF.
	self should: [(alien signedLongAt: 1) = 16r7FFFFFFF].
	alien unsignedLongAt: 1 put: 16r80000000.
	self should: [(alien signedLongAt: 1) = -2147483648].
	alien unsignedLongAt: 1 put: 16r80000001.
	self should: [(alien signedLongAt: 1) = -2147483647].
	alien unsignedLongAt: 1 put: 16r7FFFFFFF.
	self should: [(alien unsignedLongAt: 1) = 16r7FFFFFFF]. 
	self should: [(alien asUnsignedLong) = 16r7FFFFFFF]. 
	alien unsignedLongAt: 1 put: 16r80000000.
	self should: [(alien unsignedLongAt: 1) = 16r80000000].
	alien unsignedLongAt: 1 put: 16r80000001.
	self should: [(alien unsignedLongAt: 1) = 16r80000001].
		alien free.