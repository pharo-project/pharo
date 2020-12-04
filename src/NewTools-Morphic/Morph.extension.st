Extension { #name : #Morph }

{ #category : #'*NewTools-Morphic' }
Morph >> inspectionMorph [
	<inspectorPresentationOrder: 90 title: 'Morph'>

	^ SpMorphPresenter new
		morph: self imageForm asMorph;
		yourself
]

{ #category : #'*NewTools-Morphic' }
Morph >> inspectorIcon [

	^ self class systemIcon
]
