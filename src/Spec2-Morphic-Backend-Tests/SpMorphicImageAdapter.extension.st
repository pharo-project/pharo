Extension { #name : #SpMorphicImageAdapter }

{ #category : #'*Spec2-Morphic-Backend-Tests' }
SpMorphicImageAdapter >> hasImageAutoscaled [
	^ self widget layout = #scaledAspect
]
