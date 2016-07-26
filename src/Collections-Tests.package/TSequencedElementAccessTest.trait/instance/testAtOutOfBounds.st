tests - element accessing
testAtOutOfBounds
	"self debug: #testAtOutOfBounds"
	self 
		should: [ self moreThan4Elements at: self moreThan4Elements size + 1 ]
		raise: Error.
	self 
		should: [ self moreThan4Elements at: -1 ]
		raise: Error