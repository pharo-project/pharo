creating - silently
silentlyNewSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames:  classVarsString category: category

	|  newClass |
	SystemAnnouncer uniqueInstance suspendAllWhile: [
		newClass := aClass 
			subclass: self newClassName 
			instanceVariableNames: ivNamesString 
			classVariableNames: classVarsString 
			poolDictionaries: '' 
			category: (self packageName, '-', category) asSymbol.
	].

	self createdClasses add: newClass.
	createdSilently add: newClass.
	^newClass
	