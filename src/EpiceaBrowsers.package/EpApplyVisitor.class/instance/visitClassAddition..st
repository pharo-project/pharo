visitor
visitClassAddition: aClassCreation

	self compiler
		evaluate: aClassCreation classAdded definitionSource;
		evaluate: aClassCreation classAdded theMetaClass definitionSource.