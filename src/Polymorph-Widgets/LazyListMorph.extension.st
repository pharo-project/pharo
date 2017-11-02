Extension { #name : #LazyListMorph }

{ #category : #'*Polymorph-Widgets' }
LazyListMorph >> themeChanged [
	self color: self theme textColor.
	super themeChanged
]
