Extension { #name : #LabelMorph }

{ #category : #'*Polymorph-Widgets' }
LabelMorph >> themeChanged [
	self color: self defaultColor.
	super themeChanged
]
