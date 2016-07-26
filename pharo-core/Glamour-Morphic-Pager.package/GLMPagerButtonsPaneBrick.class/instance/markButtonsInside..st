buttons-updating
markButtonsInside: aBounds
	|index boundsLeft boundsRight firstFound lastFound |	
	self flag: 'refactor me'.
	
	firstFound := false.
	lastFound := false.
	index := 0.
	
	(pagerModel size = 0) ifTrue: [ ^ self ].
	boundsLeft := aBounds x.
	boundsRight := aBounds y.
	
	self buttons do: [ :indicator |
		index := index + 1.
		(self isScrollMustBeChecked: index withLeft: boundsLeft andRight: boundsRight)
			ifTrue: [
				indicator check.
				firstFound ifFalse: [
					pagerModel virtualFirstVisiblePageIndex: index.
					firstFound := true.
				]
			]
			ifFalse: [
				"uncheck only if not focused"
				indicator isFocused ifFalse: [ indicator uncheck ].
				(firstFound & (lastFound = false)) ifTrue: [
					pagerModel virtualLastVisiblePageIndex: index - 1.
					lastFound := true.
				]
			].
	].
	lastFound ifFalse: [
		pagerModel virtualLastVisiblePageIndex: index.
		lastFound := true.
	]
	
		
