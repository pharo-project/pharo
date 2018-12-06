Extension { #name : #TextMorphForEditView }

{ #category : #'*Polymorph-Widgets-Core' }
TextMorphForEditView >> themeChanged [
	self color: self defaultColor.
	super themeChanged
]
