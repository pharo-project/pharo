tests
testMethodNotFoundAfterRemoval
	"Tests that serializer does not tolarate when the method was removed between serialization and materialization"
	
	| classOrTrait |
	classOrTrait := self newClassOrTrait.
	classOrTrait duringTestCompileSilently: 'you'.
	
	self serialize: classOrTrait >> #you.
	classOrTrait removeSelectorSilently: #you.
	
	self 
		should: [self materialized] 
		raise: FLMethodNotFound
		whoseDescriptionIncludes: classOrTrait name, '>>#you' 
		description: 'Should raise an error when method is not present.' 
	
