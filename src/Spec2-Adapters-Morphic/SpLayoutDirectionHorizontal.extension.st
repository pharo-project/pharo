Extension { #name : #SpLayoutDirectionHorizontal }

{ #category : #'*Spec2-Adapters-Morphic' }
SpLayoutDirectionHorizontal >> addPadding: aNumber toMorph: aMorph into: anAdapter [

	^ anAdapter 
		addHorizontalPadding: aNumber 
		toMorph: aMorph
]

{ #category : #'*Spec2-Adapters-Morphic' }
SpLayoutDirectionHorizontal >> newSplitter [
	^ SpPanedResizerMorph newHorizontal
]

{ #category : #'*Spec2-Adapters-Morphic' }
SpLayoutDirectionHorizontal >> setRigidityOfNonExpendedMorph: aMorph [ 
	aMorph 
		vResizing: #spaceFill; 
		hResizing: #rigid
]
