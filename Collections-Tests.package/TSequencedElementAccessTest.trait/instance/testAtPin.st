tests - element accessing
testAtPin
	"self debug: #testAtPin"
	self assert: (self moreThan4Elements atPin: 2) = self moreThan4Elements second.
	self assert: (self moreThan4Elements atPin: 99) = self moreThan4Elements last.
	self assert: (self moreThan4Elements atPin: -99) = self moreThan4Elements first