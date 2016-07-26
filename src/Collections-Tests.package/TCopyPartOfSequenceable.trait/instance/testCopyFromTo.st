tests - copying part of sequenceable
testCopyFromTo
	| result  index collection |
	collection := self collectionWithoutEqualElements .
	index :=self indexInForCollectionWithoutDuplicates .
	result := collection   copyFrom: index  to: collection  size .
	
	"verify content of 'result' : "
	1 to: result size do:
		[:i | 
		self assert: (result at:i)=(collection  at: (i + index - 1))].
	
	"verify size of 'result' : "
	self assert: result size = (collection  size - index + 1).