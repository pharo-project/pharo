tests - concatenation
testConcatenationWithEmpty
	| result |
	result := self firstCollection , self empty.
	self assert: result = self firstCollection