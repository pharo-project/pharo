Extension { #name : #SpLayoutDirectionVertical }

{ #category : #'*Spec2-Adapters-Morphic' }
SpLayoutDirectionVertical >> addPadding: aNumber toMorph: aMorph into: anAdapter [

	^ anAdapter 
		addVerticalPadding: aNumber 
		toMorph: aMorph
]

{ #category : #'*Spec2-Adapters-Morphic' }
SpLayoutDirectionVertical >> newSplitter [
	^ SpPanedResizerMorph newVertical
]

{ #category : #'*Spec2-Adapters-Morphic' }
SpLayoutDirectionVertical >> recalculatePages: anAdaptor [

	^ anAdaptor recalculatePageHeights
]

{ #category : #'*Spec2-Adapters-Morphic' }
SpLayoutDirectionVertical >> setRigidityOfNonExpandedMorph: aMorph [ 
	aMorph 
		hResizing: #spaceFill; 
		vResizing: #rigid
]
