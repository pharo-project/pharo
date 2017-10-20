tests - arithmetic
testCrossSumBase
	"self run: #testCrossSumBase"

	self assert: (
		((-20 to: 20) collect: [:each | each crossSumBase: 10]) asArray = 
		#(2 10 9 8 7 6 5 4 3 2 1 9 8 7 6 5 4 3 2 1 0 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9 10 2)).
	self assert: (
		((-20 to: 20) collect: [:each | each crossSumBase: 2]) asArray = 
		#(2 3 2 2 1 4 3 3 2 3 2 2 1 3 2 2 1 2 1 1 0 1 1 2 1 2 2 3 1 2 2 3 2 3 3 4 1 2 2 3 2)).
	self should: [10 crossSumBase: 1] raise: AssertionFailure