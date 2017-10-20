tests - equality
testEqualSignIsTrueForEmptyButNonIdenticalCollections
	"self debug: #testEqualSignIsTrueForNonIdenticalButEqualCollections"
		
	self assert: (self empty = self empty copy). 
	self assert: (self empty copy = self empty).
	self assert: (self empty copy = self empty copy).
		
