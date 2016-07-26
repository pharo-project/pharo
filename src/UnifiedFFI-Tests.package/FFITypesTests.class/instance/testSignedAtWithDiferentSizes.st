tests
testSignedAtWithDiferentSizes
	"This is tests for issue 17757" 
	| ref|
	
	ref := ByteArray new: 4.
	ref integerAt: 1 put: -10 size: 1 signed: true.
	self assert: (ref integerAt: 1 size: 1 signed: true) = -10.
	
	ref := ByteArray new: 4.
	ref integerAt: 1 put: -10 size: 4 signed: true.
	self assert: (ref integerAt: 1 size: 4 signed: true) = -10.
	
	ref := ByteArray new: 8.
	ref integerAt: 1 put: -10 size: 8 signed: true.
	self assert: (ref integerAt: 1 size: 8 signed: true) = -10