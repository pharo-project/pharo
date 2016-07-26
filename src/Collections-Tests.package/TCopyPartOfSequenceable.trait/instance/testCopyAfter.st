tests - copying part of sequenceable
testCopyAfter
	| result index collection |
	collection := self collectionWithoutEqualElements .
	index:= self indexInForCollectionWithoutDuplicates .
	result := collection   copyAfter: (collection  at:index ).
	
	"verifying content: "
	(1) to: result size do: 
		[:i |
		self assert: (collection   at:(i + index ))=(result at: (i))].

	"verify size: "
	self assert: result size = (collection   size - index).