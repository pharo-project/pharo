Extension { #name : #SpLayoutDirectionVertical }

{ #category : #'*Spec2-Adapters-Morphic' }
SpLayoutDirectionVertical >> newSplitter [
	^ SpPanedResizerMorph newVertical
]

{ #category : #'*Spec2-Adapters-Morphic' }
SpLayoutDirectionVertical >> setRigidityOfNonExpendedMorph: aMorph [ 
	aMorph 
		hResizing: #spaceFill; 
		vResizing: #rigid
]
