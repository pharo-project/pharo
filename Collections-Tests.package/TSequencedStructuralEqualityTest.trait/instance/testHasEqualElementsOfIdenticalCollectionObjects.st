tests - equality
testHasEqualElementsOfIdenticalCollectionObjects
	"self debug: #testHasEqualElementsOfIdenticalCollectionObjects"
	
	self assert: (self empty hasEqualElements: self empty). 
	self assert: (self nonEmpty hasEqualElements: self nonEmpty). 
	