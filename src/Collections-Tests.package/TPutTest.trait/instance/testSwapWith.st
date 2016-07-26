tests - puting with indexes
testSwapWith
	"self debug: #testSwapWith"
	| result index |
	index := self indexArray anyOne.
	result:= self nonEmpty copy .
	result swap: index with: 1.
	self assert: (result at: index) = (self nonEmpty at:1).
	self assert: (result at: 1) = (self nonEmpty at: index).
	