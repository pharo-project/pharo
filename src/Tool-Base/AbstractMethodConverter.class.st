"
An AbstractMessageConverter is a wrapper used to switch which information to retrieve from a method.

This way, you can avoid switch case, you just have to use set the correct filter
"
Class {
	#name : #AbstractMethodConverter,
	#superclass : #Object,
	#instVars : [
		'method'
	],
	#category : #Tool-Base
}

{ #category : #'instance creation' }
AbstractMethodConverter class >> method: aMessage [

	^ self new
		method: aMessage;
		yourself
]

{ #category : #protocol }
AbstractMethodConverter >> getText [

	method ifNil: [ ^ '' ].	
	^ self internalGetText
]

{ #category : #protocol }
AbstractMethodConverter >> getTextFor: aMethod [

	method := aMethod.
	method ifNil: [ ^ '' ].
	
	^ self internalGetText
]

{ #category : #private }
AbstractMethodConverter >> internalGetText [

	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractMethodConverter >> method [

	^ method
]

{ #category : #accessing }
AbstractMethodConverter >> method: aMessage [

	method := aMessage
]

{ #category : #initialization }
AbstractMethodConverter >> shouldShout [

	^ false
]
