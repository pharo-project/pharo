creating
newClassNamed: aString subclassOf: aClass instanceVariableNames: ivNamesString classVariableNames:  classVarsString 
	^self 
		newClassNamed: aString
		subclassOf: aClass 
		instanceVariableNames: ivNamesString 
		classVariableNames: classVarsString 
		category:  (self packageName, '-', self defaultCategoryPostfix) asSymbol.