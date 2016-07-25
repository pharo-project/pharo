testing
testOrganizationDefinition
	| definition |
	definition := MCOrganizationDefinition categories: 
					(self mockPackage packageSet systemCategories).
	writer visitOrganizationDefinition: definition.
	self assertContentsOf: stream match: self expectedOrganizationDefinition.
	self assertAllChunksAreWellFormed.