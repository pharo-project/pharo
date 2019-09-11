"
I am an eye element for sets, showing appropriate thing with indexes

"
Class {
	#name : #SetEyeElement,
	#superclass : #IndexedEyeElement,
	#instVars : [
		'externalIndex'
	],
	#category : #'Spec2-Inspector-Element'
}

{ #category : #'instance creation' }
SetEyeElement class >> host: anObject index: index externalIndex: externalIndex [
	^ (self host: anObject)
		index: index;
		externalIndex: externalIndex;
		yourself
]

{ #category : #accessing }
SetEyeElement >> accessorCode [
	^ '(self array at: ', self index asString, ')'
]

{ #category : #accessing }
SetEyeElement >> externalIndex [
	^ externalIndex
]

{ #category : #accessing }
SetEyeElement >> externalIndex: anObject [
	externalIndex := anObject
]

{ #category : #accessing }
SetEyeElement >> label [
	^ self externalIndex printString
]

{ #category : #action }
SetEyeElement >> save: aValue [
	| oldElement |
	oldElement := self host array at: self index.
	self host 
		remove: oldElement;
		add: aValue.
]

{ #category : #accessing }
SetEyeElement >> value [
	^ self host array at: self index
]
