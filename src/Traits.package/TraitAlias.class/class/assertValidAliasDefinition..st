instance creation
assertValidAliasDefinition: anArrayOfAssociations
	"Throw an exceptions if the alias definition is not valid.
	It is expected to be a collection of associations and
	the number of arguments of the alias selector has to
	be the same as the original selector."

	((anArrayOfAssociations isKindOf: Collection) and: [
		anArrayOfAssociations allSatisfy: [:each |
			each isKindOf: Association]]) ifFalse: [
		TraitCompositionException signal: 'Invalid alias definition: Not a collection of associations.'].
	
	(anArrayOfAssociations allSatisfy: [:association |
		(association key numArgs = association value numArgs and: [
			(association key numArgs = -1) not])]) ifFalse: [
		TraitCompositionException signal: 'Invalid alias definition: Alias and original selector have to have the same number of arguments.']