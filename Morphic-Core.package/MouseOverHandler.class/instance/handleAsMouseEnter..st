event handling - private
handleAsMouseEnter: anEvent 

	| asMouseEnterEvent |
	
	asMouseEnterEvent := anEvent asMouseEnter.
	enteredMorphs := enteredMorphs contents.
	enteredMorphs reverseDo: [ :anEnteredMorph | 
		self inform: asMouseEnterEvent to: anEnteredMorph originatedFrom: anEvent ifNotFocusedDo: [] ]