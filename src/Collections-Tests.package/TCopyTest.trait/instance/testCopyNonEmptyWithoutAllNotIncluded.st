tests - copy
testCopyNonEmptyWithoutAllNotIncluded
	"self debug: #testCopyNonEmptyWithoutAllNotIncluded"
	| res |
	res := self nonEmpty copyWithoutAll: self collectionNotIncluded.
	"here we do not test the size since for a non empty set we would get a problem.
	Then in addition copy is not about duplicate management. The element should 
	be in at the end."
	self nonEmpty do: [ :each | self assert: (res includes: each) ]