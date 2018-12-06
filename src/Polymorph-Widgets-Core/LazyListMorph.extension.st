Extension { #name : #LazyListMorph }

{ #category : #'*Polymorph-Widgets-Core' }
LazyListMorph >> themeChanged [
	self color: self theme textColor.
	super themeChanged
]
