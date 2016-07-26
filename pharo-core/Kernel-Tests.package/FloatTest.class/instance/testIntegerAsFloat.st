tests - conversion
testIntegerAsFloat
	"assert IEEE 754 round to nearest even mode is honoured"
	
	self deny: 16r1FFFFFFFFFFFF0801 asFloat = 16r1FFFFFFFFFFFF0800 asFloat. "this test is on 65 bits"
	self deny: 16r1FFFFFFFFFFFF0802 asFloat = 16r1FFFFFFFFFFFF0800 asFloat. "this test is on 64 bits"
	self assert: 16r1FFFFFFFFFFF1F800 asFloat = 16r1FFFFFFFFFFF20000 asFloat. "nearest even is upper"
	self assert: 16r1FFFFFFFFFFFF0800 asFloat = 16r1FFFFFFFFFFFF0000 asFloat. "nearest even is lower"
