copying
duplicateClassWithNewName: aSymbol
	| copysName class newDefinition |
	copysName := aSymbol asSymbol.
	copysName = self name
		ifTrue: [ ^ self ].
	(self environment includesKey: copysName)
		ifTrue: [ ^ self error: copysName , ' already exists' ].
	newDefinition := self definition copyReplaceAll: self name with: copysName.
	class := self class compiler source: newDefinition; logged: true; evaluate.
	class classSide instanceVariableNames: self classSide instanceVariablesString.
	class copyAllCategoriesFrom: self.
	class class copyAllCategoriesFrom: self class.
	^ class