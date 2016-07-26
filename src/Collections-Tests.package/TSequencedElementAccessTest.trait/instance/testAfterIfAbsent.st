tests - element accessing
testAfterIfAbsent
	"self debug: #testAfterIfAbsent"
	self assert: (self moreThan4Elements 
			after: (self moreThan4Elements at: 1)
			ifAbsent: [ 33 ]) = (self moreThan4Elements at: 2).
	self assert: (self moreThan4Elements 
			after: (self moreThan4Elements at: self moreThan4Elements size)
			ifAbsent: [ 33 ]) = 33.
	self assert: (self moreThan4Elements 
			after: self elementNotInForElementAccessing
			ifAbsent: [ 33 ]) = 33