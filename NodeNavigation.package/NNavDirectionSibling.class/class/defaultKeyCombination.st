builder
defaultKeyCombination
	^NNavNavigation useArrowsShortcuts 
		ifTrue: [ Character arrowRight meta shift  ]
		ifFalse: [ $u meta shift  ]