creating - silently
silentlyNewSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames:  classVarsString package: packageName

	|  newClass |
	SystemAnnouncer uniqueInstance suspendAllWhile: [
		newClass := aClass 
			subclass: self newClassName 
			instanceVariableNames: ivNamesString 
			classVariableNames: classVarsString 
			poolDictionaries: '' 
			category: packageName asSymbol.
	].

	self createdClasses add: newClass.
	createdSilently add: newClass.
	^newClass
	