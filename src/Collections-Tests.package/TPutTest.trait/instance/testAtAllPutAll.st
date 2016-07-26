tests - puting with indexes
testAtAllPutAll

	| aValueArray |
	aValueArray := self valueArray .
	self nonEmpty atAll: self indexArray putAll: aValueArray  .
	
	1 to: self indexArray size do:
		[:i |
		self assert: (self nonEmpty at:(self indexArray at: i))= (aValueArray  at:i) ]