builder
defaultKeyCombination
	^NNavNavigation useArrowsShortcuts 
		ifTrue: [ Character arrowLeft meta shift ]
		ifFalse: [ $p meta shift  ]

