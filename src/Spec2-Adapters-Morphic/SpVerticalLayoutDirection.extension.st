Extension { #name : #SpVerticalLayoutDirection }

{ #category : #'*Spec2-Adapters-Morphic' }
SpVerticalLayoutDirection >> addHomogeneousToMorph: aMorph into: anAdapter [
	
	^ anAdapter addVerticalHomogeneousToMorph: aMorph
]

{ #category : #'*Spec2-Adapters-Morphic' }
SpVerticalLayoutDirection >> addPadding: aNumber toMorph: aMorph into: anAdapter [

	^ anAdapter 
		addVerticalPadding: aNumber 
		toMorph: aMorph
]

{ #category : #'*Spec2-Adapters-Morphic' }
SpVerticalLayoutDirection >> newSplitter [
	^ SpPanedResizerMorph newVertical
]

{ #category : #'*Spec2-Adapters-Morphic' }
SpVerticalLayoutDirection >> recalculatePages: anAdaptor [

	^ anAdaptor recalculatePageHeights
]

{ #category : #'*Spec2-Adapters-Morphic' }
SpVerticalLayoutDirection >> setRigidityOfNonExpandedMorph: aMorph [

	aMorph 
		hResizing: #spaceFill; 
		vResizing: #rigid
]
