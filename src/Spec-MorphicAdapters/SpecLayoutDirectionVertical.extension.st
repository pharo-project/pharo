Extension { #name : #SpecLayoutDirectionVertical }

{ #category : #'*Spec-MorphicAdapters' }
SpecLayoutDirectionVertical >> setRigidityOfNonExpendedMorph: aMorph [ 
	aMorph 
		hResizing: #spaceFill; 
		vResizing: #rigid
]
