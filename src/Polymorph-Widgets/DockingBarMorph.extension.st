Extension { #name : #DockingBarMorph }

{ #category : #'*Polymorph-Widgets' }
DockingBarMorph >> themeChanged [
	self
		color: self theme settings derivedMenuColor;
		borderColor: self theme menuBorderColor.
	super themeChanged
]
