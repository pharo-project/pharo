Extension { #name : #SpecLayoutDirectionHorizontal }

{ #category : #'*Spec-MorphicAdapters' }
SpecLayoutDirectionHorizontal >> setRigidityOfNonExpendedMorph: aMorph [ 
	aMorph 
		vResizing: #spaceFill; 
		hResizing: #rigid
]
