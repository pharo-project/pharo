testing
testTraitsAccessor
	"self debug: #testTraitsAccessor"
	self assert: self c1 traits isEmpty.
	self assert: (self c2 traits hasEqualElements: (Array with: self t5))