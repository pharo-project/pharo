tests - creation
testWithWith
	"self debug: #testWithWith"
	
	| aCol collection element1 element2 |
	collection := self collectionMoreThan5Elements asOrderedCollection copyFrom: 1 to: 2  .
	element1 := collection at: 1.
	element2 := collection at:2.
	
	aCol := self collectionClass with: element1  with: element2 .
	self assert: (aCol occurrencesOf: element1 ) = ( collection occurrencesOf: element1).
	self assert: (aCol occurrencesOf: element2 ) = ( collection occurrencesOf: element2).
	
	