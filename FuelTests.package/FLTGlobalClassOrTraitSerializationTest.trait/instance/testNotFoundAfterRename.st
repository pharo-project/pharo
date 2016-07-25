tests
testNotFoundAfterRename
	"Tests that serializer does not tolarate when the class was renamed between serialization and materialization"
	
	| classOrTrait |
	classOrTrait := self newClassOrTrait.
	
	self serialize: classOrTrait.
	classOrTrait renameSilently: (classOrTrait name, 'Renamed') asSymbol.
	
	self should: [self materialized] raise: FLClassNotFound.