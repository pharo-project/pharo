Extension { #name : #ProgressBarState }

{ #category : #'*Spec-MorphicAdapters' }
ProgressBarState >> customizeMorphicBar: aProgressBarMorph [
	self subclassResponsibility
]

{ #category : #'*Spec-MorphicAdapters' }
ProgressBarState >> progressBarMorph [
	^ ProgressBarMorph
]
