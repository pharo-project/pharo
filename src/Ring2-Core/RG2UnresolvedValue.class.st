Class {
	#name : #RG2UnresolvedValue,
	#superclass : #Object,
	#instVars : [
		'default'
	],
	#category : #'Ring2-Core-Kernel'
}

{ #category : #'as yet unclassified' }
RG2UnresolvedValue class >> recursive [

	| unresolvedValue |
	
	unresolvedValue := self new.
	unresolvedValue default: unresolvedValue.
	^ unresolvedValue.
]

{ #category : #accessing }
RG2UnresolvedValue >> default [
	^ default
]

{ #category : #accessing }
RG2UnresolvedValue >> default: anObject [
	default := anObject
]

{ #category : #'as yet unclassified' }
RG2UnresolvedValue >> isRing2FullyResolved [

	^ false
]

{ #category : #testing }
RG2UnresolvedValue >> isRing2FullyUnresolved [

	^ true
]

{ #category : #'as yet unclassified' }
RG2UnresolvedValue >> isRing2Resolved [

	^ false
]

{ #category : #'as yet unclassified' }
RG2UnresolvedValue >> orDefaultForUnresolved [

	^ self default
]

{ #category : #'as yet unclassified' }
RG2UnresolvedValue >> referencedBehaviors [

	^ #()
]

{ #category : #accessing }
RG2UnresolvedValue >> value [

	^ self default
]
