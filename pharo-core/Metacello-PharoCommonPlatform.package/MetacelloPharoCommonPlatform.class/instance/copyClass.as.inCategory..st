reflection
copyClass: oldClass as: newName inCategory: newCategoryName
	| copysName class newDefinition |
	copysName := newName asSymbol.
	copysName = oldClass name
		ifTrue: [ ^ oldClass ].
	(Smalltalk includesKey: copysName)
		ifTrue: [ ^ self error: copysName , ' already exists' ].
	newDefinition := oldClass definition copyReplaceAll: '#' , oldClass name asString with: '#' , copysName asString printString.
	newDefinition := newDefinition
		copyReplaceAll: 'category: ' , (SystemOrganization categoryOfElement: oldClass name) asString printString
		with: 'category: ' , newCategoryName printString.
	class := self compiler evaluate: newDefinition logged: true.
	class class instanceVariableNames: oldClass class instanceVariablesString.
	class copyAllCategoriesFrom: oldClass.
	class class copyAllCategoriesFrom: oldClass class.
	class category: newCategoryName.
	^ class