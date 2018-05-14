"
I am DoubleLink, the elementary part of a DoubleLinkedList.

I hold a value, as well as a link to my successor (nextLink) and to my predecessor (previousLink) - both can be nil.
"
Class {
	#name : #DoubleLink,
	#superclass : #Object,
	#instVars : [
		'value',
		'nextLink',
		'previousLink'
	],
	#category : #'Collections-DoubleLinkedList'
}

{ #category : #'instance creation' }
DoubleLink class >> value: anObject [
	^ self new
		value: anObject;
		yourself
]

{ #category : #converting }
DoubleLink >> asDoubleLink [
	^ self
]

{ #category : #initialize }
DoubleLink >> clearLinks [
	nextLink := previousLink := nil
]

{ #category : #testing }
DoubleLink >> hasPredecessor [
	^ previousLink notNil
]

{ #category : #testing }
DoubleLink >> hasSuccessor [
	^ nextLink notNil
]

{ #category : #accessing }
DoubleLink >> nextLink [
	^ nextLink
]

{ #category : #accessing }
DoubleLink >> nextLink: anObject [
	nextLink := anObject
]

{ #category : #accessing }
DoubleLink >> previousLink [
	^ previousLink
]

{ #category : #accessing }
DoubleLink >> previousLink: anObject [
	previousLink := anObject
]

{ #category : #evaluating }
DoubleLink >> value [
	^ value
]

{ #category : #accessing }
DoubleLink >> value: anObject [
	value := anObject
]
