tests
testExactSqrt
	| four hundredth tenth two |
	four := 4.0s1.
	two := four sqrt.
	self assert: two = 2.
	self assert: (two class = four class and: [two scale = four scale]).
	hundredth := 0.01s2.
	tenth := hundredth sqrt.
	self assert: tenth * 10 = 1.
	self assert: (tenth class = hundredth class and: [tenth scale = hundredth scale]).