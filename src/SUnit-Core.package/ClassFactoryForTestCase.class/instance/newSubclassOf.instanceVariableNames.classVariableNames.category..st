creating
newSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames:  classVarsString category: category
	^self 
		newSubclassOf: aClass 
		uses: { } 
		instanceVariableNames: ivNamesString 
		classVariableNames: classVarsString 
		category: category.