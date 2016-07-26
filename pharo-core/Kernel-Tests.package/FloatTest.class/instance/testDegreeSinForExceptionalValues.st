tests - mathematical functions
testDegreeSinForExceptionalValues
	self assert: Float nan degreeSin isNaN.
	self assert: Float infinity degreeSin isNaN.
	self assert: Float infinity negated degreeSin isNaN.