creating
newSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames:  classVarsString 
	^self 
		newSubclassOf: aClass 
		uses: { }
		instanceVariableNames: ivNamesString 
		classVariableNames: classVarsString 
		category: self defaultCategoryPostfix