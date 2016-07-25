tests - testing
testIncludes
	self assert:
			((1 to: 4)
				allSatisfy: [:i | matrix1 includes: i])
