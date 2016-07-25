tests
testCopy
	"A copy of an RelationSet should give us a regular Set."

	| movie |
	
	movie := SlotExampleMovie new.
	self assert: movie actors isCollection.
	self assert: movie actors copy isCollection.
	self deny: movie actors copy class = RelationSet