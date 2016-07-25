tests - element accessing
testAfter
	"self debug: #testAfter"
	self assert: (self moreThan4Elements after: (self moreThan4Elements at: 1)) = (self moreThan4Elements at: 2).
	self 
		should: 
			[ self moreThan4Elements after: (self moreThan4Elements at: self moreThan4Elements size) ]
		raise: Error.
	self 
		should: [ self moreThan4Elements after: self elementNotInForElementAccessing ]
		raise: Error