Class {
	#name : #SpCodeObjectInteractionModel,
	#superclass : #SpCodeInteractionModel,
	#instVars : [
		'object'
	],
	#category : #'Spec2-Code'
}

{ #category : #'instance creation' }
SpCodeObjectInteractionModel class >> on: anObject [

	^ self new object: anObject
]

{ #category : #accessing }
SpCodeObjectInteractionModel >> behavior [

	^ self object class
]

{ #category : #binding }
SpCodeObjectInteractionModel >> bindingOf: aString [

	^ self doItReceiver bindingOf: aString
]

{ #category : #accessing }
SpCodeObjectInteractionModel >> doItReceiver [

	^ self object
]

{ #category : #testing }
SpCodeObjectInteractionModel >> hasBindingOf: aString [

	^ self doItReceiver hasBindingOf: aString
]

{ #category : #testing }
SpCodeObjectInteractionModel >> isForScripting [

	^ true
]

{ #category : #accessing }
SpCodeObjectInteractionModel >> object [

	^ object
]

{ #category : #accessing }
SpCodeObjectInteractionModel >> object: anObject [

	object := anObject
]
