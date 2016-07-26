tests - indexable access
testAtWrap
	"self debug: #testAt"
	"
	self assert: (self accessCollection at: 1) = 1.
	self assert: (self accessCollection at: 2) = 2.
	"
	| index |
	index := self accessCollection indexOf: self elementInForIndexAccessing.
	self assert: (self accessCollection atWrap: index) = self elementInForIndexAccessing.
	self assert: (self accessCollection atWrap: index + self accessCollection size) = self elementInForIndexAccessing.
	self assert: (self accessCollection atWrap: index - self accessCollection size) = self elementInForIndexAccessing.
	self assert: (self accessCollection atWrap: 1 + self accessCollection size) = (self accessCollection at: 1)