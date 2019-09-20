"
TickingSpecWindow new openInWorld
"
Class {
	#name : #SpTickingSpecWindow,
	#superclass : #SpWindow,
	#category : #'Spec2-Adapters-Morphic-Support'
}

{ #category : #stepping }
SpTickingSpecWindow >> step [
	self model step.
]

{ #category : #stepping }
SpTickingSpecWindow >> stepTime [

	^ self model stepTime
]

{ #category : #stepping }
SpTickingSpecWindow >> wantsSteps [
	^ true
]
