copying
duplicateClassWithNewName: aSymbol
	| copysName class |
	copysName := aSymbol asSymbol.
	copysName = self name
		ifTrue: [ ^ self ].
	(self environment includesKey: copysName)
		ifTrue: [ ^ self error: copysName , ' already exists' ].
	class := self superclass
		subclass: copysName
		instanceVariableNames: self instanceVariablesString
		classVariableNames: self classVariablesString
		package: self package name.
	class classSide
		instanceVariableNames: self classSide instanceVariablesString.
	class copyAllCategoriesFrom: self.
	class class copyAllCategoriesFrom: self class.
	^ class