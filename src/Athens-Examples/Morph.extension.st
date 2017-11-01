Extension { #name : #Morph }

{ #category : #'*Athens-Examples' }
Morph >> openInSceneView [
	^ AthensSceneView new
		scene: (AthensMorphScene new morph:self);
		openInWindow
]
