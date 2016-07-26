tests - element accessing
testFirstSecondThird
	"self debug: #testFirstSecondThird"
	self assert: self moreThan4Elements first = (self moreThan4Elements at: 1).
	self assert: self moreThan4Elements second = (self moreThan4Elements at: 2).
	self assert: self moreThan4Elements third = (self moreThan4Elements at: 3).
	self assert: self moreThan4Elements fourth = (self moreThan4Elements at: 4)