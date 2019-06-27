"
I am an eye element for sets, showing appropriate thing with indexes

"
Class {
	#name : #SpSetEyeElement,
	#superclass : #SpIndexedEyeElement,
	#instVars : [
		'externalIndex'
	],
	#category : #'Spec2-Inspector-Element'
}

{ #category : #'instance creation' }
SpSetEyeElement class >> host: anObject index: index externalIndex: externalIndex [
	^ (self host: anObject)
		index: index;
		externalIndex: externalIndex;
		yourself
]

{ #category : #accessing }
SpSetEyeElement >> accessorCode [
	^ '(self array at: ', self index asString, ')'
]

{ #category : #accessing }
SpSetEyeElement >> externalIndex [
	^ externalIndex
]

{ #category : #accessing }
SpSetEyeElement >> externalIndex: anObject [
	externalIndex := anObject
]

{ #category : #accessing }
SpSetEyeElement >> label [
	^ self externalIndex printString
]

{ #category : #action }
SpSetEyeElement >> save: aValue [
	| oldElement |
	oldElement := self host array at: self index.
	self host 
		remove: oldElement;
		add: aValue.
]

{ #category : #accessing }
SpSetEyeElement >> value [
	^ self host array at: self index
]
