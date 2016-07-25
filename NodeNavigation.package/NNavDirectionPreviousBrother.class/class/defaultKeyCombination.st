builder
defaultKeyCombination
	^NNavNavigation useArrowsShortcuts 
		ifTrue: [Character arrowLeft meta  ]
		ifFalse: [ $u meta  ]
