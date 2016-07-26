tests - element accessing
testBefore
	"self debug: #testBefore"
	self assert: (self moreThan4Elements before: (self moreThan4Elements at: 2)) = (self moreThan4Elements at: 1).
	self 
		should: [ self moreThan4Elements before: (self moreThan4Elements at: 1) ]
		raise: Error.
	self 
		should: [ self moreThan4Elements before: 66 ]
		raise: Error