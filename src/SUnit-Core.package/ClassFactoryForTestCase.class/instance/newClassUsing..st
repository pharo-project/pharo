creating
newClassUsing: aTraitComposition
	^self 
		newSubclassOf: self defaultSuperclass 
		uses: aTraitComposition 
		instanceVariableNames: '' 
		classVariableNames: ''
		category: self defaultCategoryPostfix.