tests
testNormalize
	"self run: #testNormalize"
	"Check normalization and conversion to/from SmallInts"

	self assert: ((SmallInteger maxVal + 1 - 1) == SmallInteger maxVal).
	self assert: (SmallInteger maxVal + 3 - 6) == (SmallInteger maxVal-3).
	self should: ((SmallInteger minVal - 1 + 1) == SmallInteger minVal).
	self assert: (SmallInteger minVal - 3 + 6) == (SmallInteger minVal+3).