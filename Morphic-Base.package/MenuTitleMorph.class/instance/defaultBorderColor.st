initialization
defaultBorderColor
	^ self theme settings flatMenu 
		ifFalse: [#inset]
		ifTrue: [self theme settings derivedMenuColor muchDarker]