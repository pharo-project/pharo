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

{ #category : #binding }
SpCodeBehaviorInteractionModel >> bindingOf: aString [

	^ self behavior bindingOf: aString
]

{ #category : #testing }
SpCodeBehaviorInteractionModel >> hasBindingOf: aString [

	^ self behavior hasBindingOf: aString
]
