creating
newSubclassOf: aClass uses: aTraitComposition instanceVariableNames: ivNamesString classVariableNames:  classVarsString poolDictionaries: poolNamesString category: category
	| newClass |
	newClass := aClass 
		subclass: self newClassName 
		uses: aTraitComposition
		instanceVariableNames: ivNamesString 
		classVariableNames: classVarsString 
		poolDictionaries: poolNamesString 
		category: (self packageName, '-', category) asSymbol.
	self createdClasses add: newClass.
	^newClass