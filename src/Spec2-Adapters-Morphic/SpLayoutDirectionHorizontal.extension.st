Extension { #name : #SpLayoutDirectionHorizontal }

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
