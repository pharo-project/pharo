Extension { #name : #DockingBarMorph }

{ #category : #'*Polymorph-Widgets' }
DockingBarMorph >> themeChanged [
	self
		color: self defaultColor;
		borderColor: self defaultBorderColor;
		borderWidth: self defaultBorderWidth.
	super themeChanged
]
