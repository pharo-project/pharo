tests - element accessing
testMiddle
	"self debug: #testMiddle"
	self assert: self moreThan4Elements middle = (self moreThan4Elements at: self moreThan4Elements size // 2 + 1)