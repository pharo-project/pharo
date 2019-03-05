Extension { #name : #ProgressBarFixed }

{ #category : #'*Spec-MorphicAdapters' }
ProgressBarFixed >> customizeMorphicBar: aProgressBarMorph [
	aProgressBarMorph value: self value
]
