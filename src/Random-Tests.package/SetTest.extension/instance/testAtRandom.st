*Random-Tests
testAtRandom
	| rand |
	rand := Random new.
	self assert: (full includes: (full atRandom: rand)).
	