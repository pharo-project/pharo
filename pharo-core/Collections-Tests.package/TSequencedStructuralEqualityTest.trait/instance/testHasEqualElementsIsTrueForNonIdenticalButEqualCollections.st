tests - equality
testHasEqualElementsIsTrueForNonIdenticalButEqualCollections
	"self debug: #testHasEqualElementsIsTrueForNonIdenticalButEqualCollections"
		
	self assert: (self empty hasEqualElements: self empty copy). 
	self assert: (self empty copy hasEqualElements: self empty).
	self assert: (self empty copy hasEqualElements: self empty copy).
		
	self assert: (self nonEmpty hasEqualElements: self nonEmpty copy). 
	self assert: (self nonEmpty copy hasEqualElements: self nonEmpty).
	self assert: (self nonEmpty copy hasEqualElements: self nonEmpty copy).