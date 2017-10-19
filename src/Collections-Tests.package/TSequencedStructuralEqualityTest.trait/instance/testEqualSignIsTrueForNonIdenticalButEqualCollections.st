tests - equality
testEqualSignIsTrueForNonIdenticalButEqualCollections
	"self debug: #testEqualSignIsTrueForNonIdenticalButEqualCollections"
		
	self assert: (self empty = self empty copy). 
	self assert: (self empty copy = self empty).
	self assert: (self empty copy = self empty copy).
		
	self assert: (self nonEmpty = self nonEmpty copy). 
	self assert: (self nonEmpty copy = self nonEmpty).
	self assert: (self nonEmpty copy = self nonEmpty copy).