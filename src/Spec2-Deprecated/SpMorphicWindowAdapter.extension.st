Extension { #name : #SpMorphicWindowAdapter }

{ #category : #'*Spec2-Deprecated' }
SpMorphicWindowAdapter >> centeredRelativeTo: aModel [
	self
		deprecated:
			'This feature is to tight to Morphic to be in Spec. If you want this feature in a Morphic specific Spec application you should apply it yourself on the widget.
	
	aWindow
			fullBounds;
			center: self adatper widget center'
		on: '2019-06-25'
		in: #Pharo8
]

{ #category : #'*Spec2-Deprecated' }
SpMorphicWindowAdapter >> modalRelativeTo: aWindow [
	self deprecated: 'Do not use this directly. Use #openModalWithSpec (and family) instead.' on: '2019-02-26' in: #Pharo8.

	self widgetDo: [ :w | aWindow spSetModal: w ]
]
