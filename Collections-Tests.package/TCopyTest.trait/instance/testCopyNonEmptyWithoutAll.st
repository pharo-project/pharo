tests - copy
testCopyNonEmptyWithoutAll
	"self debug: #testCopyNonEmptyWithoutAll"
	| res |
	res := self nonEmpty copyWithoutAll: self collectionWithElementsToRemove.
	"here we do not test the size since for a non empty set we would get a problem.
	Then in addition copy is not about duplicate management. The element should 
	be in at the end."
	self collectionWithElementsToRemove do: [ :each | self deny: (res includes: (each)) ].
	self nonEmpty do: 
		[ :each | 
		(self collectionWithElementsToRemove includes: each) ifFalse: [ self assert: (res includes: each) ] ]