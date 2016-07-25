instance creation
with: aTraitComposition aliases: anArrayOfAssociations
	self assertValidAliasDefinition: anArrayOfAssociations.
	^self new
		subject: aTraitComposition;
		aliases: anArrayOfAssociations;
		yourself