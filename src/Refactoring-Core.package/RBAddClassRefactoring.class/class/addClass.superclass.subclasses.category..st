instance creation
addClass: aName superclass: aClass subclasses: aCollection category: aSymbol
	^self new
		addClass: aName
		superclass: aClass
		subclasses: aCollection
		category: aSymbol