Extension { #name : #SpProgressBarState }

{ #category : #'*Spec2-Adapters-Morphic' }
SpProgressBarState >> customizeMorphicBar: aProgressBarMorph [
	self subclassResponsibility
]

{ #category : #'*Spec2-Adapters-Morphic' }
SpProgressBarState >> progressBarMorph [
	^ ProgressBarMorph
]
