Class {
	#name : #SpCodeMethodInteractionModel,
	#superclass : #SpCodeInteractionModel,
	#instVars : [
		'method'
	],
	#category : #'Spec2-Code'
}

{ #category : #'instance creation' }
SpCodeMethodInteractionModel class >> on: aMethod [

	^ self new method: aMethod
]

{ #category : #accessing }
SpCodeMethodInteractionModel >> behavior [

	^ self method ifNotNil: [ self method methodClass ]
]

{ #category : #accessing }
SpCodeMethodInteractionModel >> doItReceiver [

	^ self behavior instanceSide
]

{ #category : #initialization }
SpCodeMethodInteractionModel >> initialize [

	super initialize.
	self setBindings: Dictionary new
]

{ #category : #accessing }
SpCodeMethodInteractionModel >> method [

	^ method
]

{ #category : #accessing }
SpCodeMethodInteractionModel >> method: aMethod [

	method := aMethod
]
