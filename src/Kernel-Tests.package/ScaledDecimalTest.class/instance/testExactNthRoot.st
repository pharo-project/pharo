tests
testExactNthRoot
	| eight thousandth tenth two |
	eight := 8.0s1.
	two := eight raisedTo: 1/3.
	self assert: two = 2.
	self assert: (two class = eight class and: [two scale = eight scale]).
	thousandth := 0.001s3.
	tenth := thousandth raisedTo: 1/3.
	self assert: tenth * 10 = 1.
	self assert: (tenth class = thousandth class and: [tenth scale = thousandth scale]).