tests - element accessing
testAtWrap
	"self debug: #testAt"
	"
	self assert: (self accessCollection at: 1) = 1.
	self assert: (self accessCollection at: 2) = 2.
	"
	| index |
	index := self moreThan4Elements indexOf: self elementInForElementAccessing.
	self assert: (self moreThan4Elements atWrap: index) = self elementInForElementAccessing.
	self assert: (self moreThan4Elements atWrap: index + self moreThan4Elements size) = self elementInForElementAccessing.
	self assert: (self moreThan4Elements atWrap: index - self moreThan4Elements size) = self elementInForElementAccessing.
	self assert: (self moreThan4Elements atWrap: 1 + self moreThan4Elements size) = (self moreThan4Elements at: 1)