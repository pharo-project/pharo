Class {
	#name : #SpCodeBehaviorInteractionModel,
	#superclass : #SpCodeInteractionModel,
	#instVars : [
		'behavior'
	],
	#category : #'Spec2-Code'
}

{ #category : #'instance creation' }
SpCodeBehaviorInteractionModel class >> on: aBehavior [

	^ self new behavior: aBehavior
]

{ #category : #accessing }
SpCodeBehaviorInteractionModel >> behavior [

	^ behavior
]

{ #category : #accessing }
SpCodeBehaviorInteractionModel >> behavior: anObject [
	behavior := anObject
]

{ #category : #initialization }
SpCodeBehaviorInteractionModel >> initialize [

	super initialize.
	self setBindings: Dictionary new
]
