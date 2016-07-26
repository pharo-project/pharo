class-factory
newSubclassNamed: nameString of: aClass  instanceVariableNames: ivNamesString classVariableNames: classVarsString
	| class |
	SystemAnnouncer uniqueInstance suspendAllWhile: [
		class := aClass
			subclass: nameString asSymbol
			instanceVariableNames: ivNamesString
			classVariableNames:  classVarsString
			category: self classFactory defaultCategory
	].
	self classFactory createdClasses add: class.
	^ class