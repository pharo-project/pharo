builder
defaultKeyCombination
	^NNavNavigation useArrowsShortcuts 
		ifTrue: [ Character arrowRight meta shift ]
		ifFalse: [ $o meta shift  ]

