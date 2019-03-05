Extension { #name : #ProgressBarIndeterminated }

{ #category : #'*Spec-MorphicAdapters' }
ProgressBarIndeterminated >> customizeMorphicBar: aProgressBarMorph [
	"Nothing to do here"
]

{ #category : #'*Spec-MorphicAdapters' }
ProgressBarIndeterminated >> progressBarMorph [
	^ IndeterminatedProgressBarMorph
]
