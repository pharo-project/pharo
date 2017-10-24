tests - testing
testIncludesAssociationNoValue

	| association dictionary |
	
	association := Association key: #key.
	
	self assert: association value isNil.
	
	dictionary := self collectionClass new.
	
	dictionary add: association.
	
	self assert: (dictionary at: #key) isNil

	
	
	