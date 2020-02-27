Class {
	#name : #RGUnresolvedValue,
	#superclass : #Object,
	#instVars : [
		'default'
	],
	#category : #'Ring-Core-Kernel'
}

{ #category : #'as yet unclassified' }
RGUnresolvedValue class >> recursive [

	| unresolvedValue |
	
	unresolvedValue := self new.
	unresolvedValue default: unresolvedValue.
	^ unresolvedValue.
]

{ #category : #accessing }
RGUnresolvedValue >> default [
	^ default
]

{ #category : #accessing }
RGUnresolvedValue >> default: anObject [
	default := anObject
]

{ #category : #'as yet unclassified' }
RGUnresolvedValue >> isRGUnresolvedValue [

	^ true
]

{ #category : #'as yet unclassified' }
RGUnresolvedValue >> isRingFullyResolved [

	^ false
]

{ #category : #testing }
RGUnresolvedValue >> isRingFullyUnresolved [

	^ true
]

{ #category : #'as yet unclassified' }
RGUnresolvedValue >> isRingResolved [

	^ false
]

{ #category : #'as yet unclassified' }
RGUnresolvedValue >> orDefaultForUnresolved [

	^ self default
]

{ #category : #'as yet unclassified' }
RGUnresolvedValue >> referencedBehaviors [

	^ #()
]

{ #category : #accessing }
RGUnresolvedValue >> value [

	^ self default
]
