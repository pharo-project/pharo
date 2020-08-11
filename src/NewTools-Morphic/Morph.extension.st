Extension { #name : #Morph }

{ #category : #'*NewTools-Morphic' }
Morph >> inspectorIcon [

	^ self class systemIcon
]

{ #category : #'*NewTools-Morphic' }
Morph >> morphInspection [
	<inspectorPresentationOrder: 90 title: 'Morph'>

	^ SpMorphPresenter new
		morph: self imageForm asMorph;
		yourself
]
