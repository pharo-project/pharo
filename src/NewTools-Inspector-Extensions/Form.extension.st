Extension { #name : #Form }

{ #category : #'*NewTools-Inspector-Extensions' }
Form >> inspectionForm [
	<inspectorPresentationOrder: 0 title: 'Form'>
	
	^ SpMorphPresenter new 
		morph: self asMorph;
		yourself
]
