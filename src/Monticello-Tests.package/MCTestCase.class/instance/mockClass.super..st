mocks
mockClass: className super: superclassName
	^ MCClassDefinition
		name:  className
		superclassName:  superclassName
		category: self mockCategoryName
		instVarNames: #()
		classVarNames: #()
		poolDictionaryNames: #()
		classInstVarNames: #()
		type: #normal
		comment: (self commentForClass: className)
		commentStamp: (self commentStampForClass: className)