tests
testindexOfStartingAt
	"self run: #testindexOfStartingAt"
	
	self assert: (#[1 2 3 4 5 6 7 8 9 10 11 1 2 3 4 5 6 7 8 0 0]  indexOf: 1 startingAt: 10) = 12.
	self assert: (#[1 2 3 4 5 6 7 8 9 10 11 1 2 3 4 5 6 7 8 0 0]  indexOf: 17 startingAt: 10) = 0.

