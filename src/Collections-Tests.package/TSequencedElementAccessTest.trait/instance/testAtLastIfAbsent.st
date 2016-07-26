tests - element accessing
testAtLastIfAbsent
	"self debug: #testAtLastIfAbsent"
	self assert: (self moreThan4Elements 
			atLast: 1
			ifAbsent: [ nil ]) = self moreThan4Elements last.
	self assert: (self moreThan4Elements 
			atLast: self moreThan4Elements size + 1
			ifAbsent: [ 222 ]) = 222