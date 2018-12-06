Extension { #name : #GroupWindowMorph }

{ #category : #'*Polymorph-Widgets-Core' }
GroupWindowMorph >> themeChanged [
	self color: self defaultColor.
	super themeChanged
]
