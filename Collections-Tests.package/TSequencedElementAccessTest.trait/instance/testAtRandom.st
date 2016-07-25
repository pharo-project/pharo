tests - element accessing
testAtRandom
	| result |
	result := self nonEmpty atRandom .
	self assert: (self nonEmpty includes: result).