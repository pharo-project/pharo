tests
testCopy
	"Elementary tests"
	self assert: 2.0 copy = 2.0.
	self assert: -0.5 copy = -0.5.
	
	"Are exceptional Floats preserved by the copy ?"
	self assert: Float nan copy isNaN.
	self assert: Float infinity copy = Float infinity.
	self assert: Float infinity negated copy = Float infinity negated.
	
	"Is the sign of zero preserved by the copy ?"
	self assert: 0.0 copy hex = 0.0 hex.
	self assert: Float negativeZero copy hex = Float negativeZero hex.