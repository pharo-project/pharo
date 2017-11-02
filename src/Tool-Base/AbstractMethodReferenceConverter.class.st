"
Abstract wrapper for method references
"
Class {
	#name : #AbstractMethodReferenceConverter,
	#superclass : #AbstractMethodConverter,
	#instVars : [
		'referencesList'
	],
	#category : 'Tool-Base'
}

{ #category : #'instance creation' }
AbstractMethodReferenceConverter class >> methodReference: aMessage referencesList: aListOfReferences [
	^ (self method: aMessage)
		referencesList: aListOfReferences;
		yourself
]

{ #category : #initialization }
AbstractMethodReferenceConverter >> initialize [

	super initialize.
	referencesList := #()
]

{ #category : #private }
AbstractMethodReferenceConverter >> priorVersionOfAMethod: aMethodReference [
	| tempList |
	
	tempList := referencesList select:[:each | 
		(each className = aMethodReference className) & 
		(each name = aMethodReference name)].
	^ tempList detect: [:each | (self versionOfAMethod: each) = ((self versionOfAMethod: aMethodReference) -1)] ifNone: [aMethodReference].
]

{ #category : #accessing }
AbstractMethodReferenceConverter >> referencesList: aListOfReferences [

	referencesList := aListOfReferences
]

{ #category : #private }
AbstractMethodReferenceConverter >> versionOfAMethod: aMethodReference [
	| tempList |
	
	tempList := referencesList select: [:each | 
		(each className = aMethodReference className) & 
		(each name = aMethodReference name)].
	tempList := tempList sort: [:m1 :m2 | m1 timeStamp < m2 timeStamp].
	^ tempList identityIndexOf: aMethodReference ifAbsent: [0]
]
