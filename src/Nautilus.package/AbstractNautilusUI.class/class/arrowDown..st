icons behavior
arrowDown: aMethod

	| methods methodsNames index |
	methods := aMethod methodClass allSubclasses flatCollect: [:each | each methods ].
	methods := methods select: [:each | each selector = aMethod selector ].
	methods size = 1
		 ifTrue: [ ^ Nautilus openOnMethod: methods first ].
	methods isEmpty
		ifTrue:[ ^ MethodWidget removeAllFromMethodsIconsCache: aMethod ].
	methodsNames := methods collect: [:each | each methodClass name, '>>#', each selector ].
	index := UIManager default chooseFrom: methodsNames.
	index = 0 ifTrue: [ ^ self ].
	^ Nautilus openOnMethod: (methods at: index)
	