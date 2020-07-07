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
SpCodeObjectInteractionModel >> bindings [

	^ bindings ifNil: [ bindings := self collectBindings ]
	
]

{ #category : #private }
SpCodeObjectInteractionModel >> collectBindings [
	| newBindings |
	
	newBindings := Dictionary new.
	self behavior ifNotNil: [ :aClass | 
		self collectBindingsOf: aClass into: newBindings ].
	self doItReceiver ifNotNil: [ :anObject |
		self collectBindingsOf: anObject class into: newBindings ].

	^ newBindings
]

{ #category : #private }
SpCodeObjectInteractionModel >> collectBindingsOf: aClass into: aDictionary [

	aClass slots do: [ :each |
		aDictionary 
			at: each name 
			put: (each read: self object) ]
]

{ #category : #accessing }
SpCodeObjectInteractionModel >> doItReceiver [

	^ self object
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

	object := anObject.
	bindings := nil
]
