tests
testInexactSqrt
	| tenth sqrt3 sqrtTenth three |
	three := 3.0s1.
	sqrt3 := three sqrt.
	self assert: sqrt3 isFloat.
	self deny: sqrt3 squared = 3.
	tenth := 0.10s2.
	sqrtTenth := tenth sqrt.
	self assert: sqrtTenth isFloat.
	self deny: sqrtTenth squared = tenth.