tests - bitLogic
testBitString
	"self debug: #testBitString"
	
	Smalltalk vm wordSize = 4
		ifTrue: [
			self assert: 2 bitString equals: '0000000000000000000000000000010'.
			self assert: -1 bitString equals: '1111111111111111111111111111111'.
			self assert: -2 bitString equals: '1111111111111111111111111111110'.
			self assert: 2 bitStringLength equals: 31 ]. 
	Smalltalk vm wordSize = 8
		ifTrue: [
			self assert: 2 bitString equals: '0000000000000000000000000000000000000000000000000000000000010'.
			self assert: -1 bitString equals: '1111111111111111111111111111111111111111111111111111111111111'.
			self assert: -2 bitString equals: '1111111111111111111111111111111111111111111111111111111111110'.
			self assert: 2 bitStringLength equals: 61 ]. 
	"32 minus 1 for immediate encoding = 31 = 30 for number + 1 for sign"
	"64 minus 3 for immediate encoding = 61 = 60 for number + 1 for sign"
	self assert: 2 bitStringLength equals: (SmallInteger maxVal highBit + 1).