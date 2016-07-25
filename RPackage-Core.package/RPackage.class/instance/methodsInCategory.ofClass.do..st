system compatibility
methodsInCategory: aString ofClass: aClass do: aBlock
	((aClass organization listAtCategoryNamed: aString) ifNil: [^self])
		do: [:sel | aBlock value: (self referenceForMethod: sel ofClass: aClass)]