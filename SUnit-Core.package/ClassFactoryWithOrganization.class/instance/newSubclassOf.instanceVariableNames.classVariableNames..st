creating
newSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames:  classVarsString 
	^self 
		newSubclassOf: aClass 
		instanceVariableNames: ivNamesString 
		classVariableNames: classVarsString 
		category:  (self packageName, '-', self defaultCategoryPostfix) asSymbol.