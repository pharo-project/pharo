Extension { #name : #GroupWindowMorph }

{ #category : #'*Polymorph-Widgets' }
GroupWindowMorph >> themeChanged [
	self color: self defaultColor.
	super themeChanged
]
