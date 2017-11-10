Extension { #name : #TextMorphForEditView }

{ #category : #'*Polymorph-Widgets' }
TextMorphForEditView >> themeChanged [
	self color: self defaultColor.
	super themeChanged
]
