literals
testNegativeZero
	self assert: (OpalCompiler evaluate: '-0.0') hex = Float negativeZero hex.