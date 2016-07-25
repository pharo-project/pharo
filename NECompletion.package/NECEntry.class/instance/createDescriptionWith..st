detail information
createDescriptionWith: anECContext 
	| clazz |
	clazz := self guessTypeWith: anECContext.
	^ clazz 
		ifNil: [ NECEntryDescription label: self label ]
		ifNotNil: 
			[ NECEntryDescription 
				label: self label
				title: clazz printString
				description: clazz comment ]