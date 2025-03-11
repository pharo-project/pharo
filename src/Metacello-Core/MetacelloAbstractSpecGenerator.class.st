Class {
	#name : 'MetacelloAbstractSpecGenerator',
	#superclass : 'Object',
	#instVars : [
		'target'
	],
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'accessing' }
MetacelloAbstractSpecGenerator >> projectSpecCreationBlock [
	^ self subclassResponsibility
]

{ #category : 'accessing' }
MetacelloAbstractSpecGenerator >> projectSpecListBlock [
	^ self subclassResponsibility
]

{ #category : 'accessing' }
MetacelloAbstractSpecGenerator >> projectSpecLookupBlock [
	^ self subclassResponsibility
]

{ #category : 'accessing' }
MetacelloAbstractSpecGenerator >> target [
	^ target
]

{ #category : 'accessing' }
MetacelloAbstractSpecGenerator >> target: anObject [
	target := anObject
]
