tests
testFourthByteArraysReturnTheCorrectValues
	"self run: #testFourthByteArraysReturnTheCorrectValues"
	
	self assert: ((#[16r3F 16r80 0 0]  floatAt:1 bigEndian: true) = 1.0).
	self assert: ((#[16rC0 0 0 0]  floatAt:1 bigEndian: true) = -2.0).

