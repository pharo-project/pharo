Extension { #name : #TextMorphForEditView }

{ #category : #'*Athens-Morphic' }
TextMorphForEditView >> drawOnAthensCanvas: anAthensCanvas [
	self useSelectionBar
		ifTrue: [self paragraph displaySelectionBarOnAthensCanvas: anAthensCanvas].
	self useExtraSelection 
		ifTrue: [self paragraph displayExtraSelectionOnAthensCanvas: anAthensCanvas].
	super drawOnAthensCanvas: anAthensCanvas.

]
