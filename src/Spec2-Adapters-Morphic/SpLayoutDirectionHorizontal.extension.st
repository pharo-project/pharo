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
SpLayoutDirectionHorizontal >> recalculatePages: anAdaptor [

	^ anAdaptor recalculatePageWidths
]

{ #category : #'*Spec2-Adapters-Morphic' }
SpLayoutDirectionHorizontal >> setRigidityOfNonExpandedMorph: aMorph [

	aMorph 
		vResizing: #spaceFill; 
		hResizing: #rigid
]
