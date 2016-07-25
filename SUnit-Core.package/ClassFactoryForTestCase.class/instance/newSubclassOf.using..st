creating
newSubclassOf: aClass using: aTraitComposition
	^self 
		newSubclassOf: aClass
		uses: aTraitComposition 
		instanceVariableNames: '' 
		classVariableNames: ''
		category: self defaultCategoryPostfix.