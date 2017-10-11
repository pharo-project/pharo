private
createNewClassNamed: aName inCategory: aCategoryName
	
	| cls |
	cls := Object subclass: aName asSymbol
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: aCategoryName.
	^ cls