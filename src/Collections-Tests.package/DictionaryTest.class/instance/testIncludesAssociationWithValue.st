tests - testing
testIncludesAssociationWithValue

	| association dictionary |
	
	association := Association key: #key value: 1.
	dictionary := self collectionClass new.
	dictionary add: association.
	
	self assert: (dictionary at: #key) = 1

	
	
	