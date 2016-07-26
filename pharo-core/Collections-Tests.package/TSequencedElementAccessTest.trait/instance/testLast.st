tests - element accessing
testLast
	"self debug: #testLast"
	self assert: self moreThan4Elements last = (self moreThan4Elements at: self moreThan4Elements size)