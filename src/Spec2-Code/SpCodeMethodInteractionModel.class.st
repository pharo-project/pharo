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
SpCodeMethodInteractionModel >> bindingOf: aString [

	^ self behavior bindingOf: aString
]

{ #category : #accessing }
SpCodeMethodInteractionModel >> doItReceiver [

	^ self behavior instanceSide
]

{ #category : #testing }
SpCodeMethodInteractionModel >> hasBindingOf: aString [

	^ self behavior hasBindingOf: aString
]

{ #category : #accessing }
SpCodeMethodInteractionModel >> method [

	^ method
]

{ #category : #accessing }
SpCodeMethodInteractionModel >> method: aMethod [

	method := aMethod
]
