utility
createNewClassNamed: aName inCategory: cat
	
	| cls |
	cls := Object subclass: aName
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: cat.
	createdClasses add: cls.
	createdCategories add: cat.
	^ cls