Extension { #name : #MorphTreeNodeMorph }

{ #category : #'*Spec-MorphicAdapters' }
MorphTreeNodeMorph >> isMorphicModel [

	^ true
]

{ #category : #'*Spec-MorphicAdapters' }
MorphTreeNodeMorph >> model [

	^ self complexContents
]
