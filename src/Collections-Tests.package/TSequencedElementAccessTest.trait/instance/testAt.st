tests - element accessing
testAt
	"self debug: #testAt"
	"
	self assert: (self accessCollection at: 1) = 1.
	self assert: (self accessCollection at: 2) = 2.
	"
	| index |
	index := self moreThan4Elements indexOf: self elementInForElementAccessing.
	self assert: (self moreThan4Elements at: index) = self elementInForElementAccessing