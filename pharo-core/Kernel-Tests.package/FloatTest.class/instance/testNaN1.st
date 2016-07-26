tests - NaN behavior
testNaN1
   	"FloatTest new testNaN1"

	self assert: Float nan == Float nan.
	self deny: Float nan = Float nan.
	"a NaN is not equal to itself."
