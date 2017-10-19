tests - creation
testWithAll
	"self debug: #testWithAll"
	
	| aCol collection |
	collection := self collectionMoreThan5Elements asOrderedCollection .
	aCol := self collectionClass withAll: collection  .
	
	collection do: [ :each | self assert: (aCol occurrencesOf: each ) = ( collection occurrencesOf: each ) ].
	
	self assert: (aCol size = collection size ).