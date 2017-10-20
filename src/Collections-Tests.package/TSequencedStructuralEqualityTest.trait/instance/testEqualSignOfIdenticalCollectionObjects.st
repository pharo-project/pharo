tests - equality
testEqualSignOfIdenticalCollectionObjects
	"self debug: #testEqualSignOfIdenticalCollectionObjects"
	
	self assert: (self empty = self empty). 
	self assert: (self nonEmpty = self nonEmpty). 
	