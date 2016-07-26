tests - element accessing
testBeforeIfAbsent
	"self debug: #testBefore"
	self assert: (self moreThan4Elements 
			before: (self moreThan4Elements at: 1)
			ifAbsent: [ 99 ]) = 99.
	self assert: (self moreThan4Elements 
			before: (self moreThan4Elements at: 2)
			ifAbsent: [ 99 ]) = (self moreThan4Elements at: 1)