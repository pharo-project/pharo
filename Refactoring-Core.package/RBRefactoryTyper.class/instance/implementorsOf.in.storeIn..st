computing types
implementorsOf: aSelector in: aClass storeIn: classes 
	(aClass directlyDefinesMethod: aSelector) 
		ifTrue: 
			[classes add: aClass.
			^self].
	aClass subclasses do: 
			[:each | 
			self 
				implementorsOf: aSelector
				in: each
				storeIn: classes]