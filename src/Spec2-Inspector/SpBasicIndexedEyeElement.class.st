"
I am an eye element for indexable fields. (basicAt:)
"
Class {
	#name : #SpBasicIndexedEyeElement,
	#superclass : #SpAbstractEyeElement,
	#instVars : [
		'index'
	],
	#category : #'Spec2-Inspector-Element'
}

{ #category : #'instance creation' }
SpBasicIndexedEyeElement class >> host: anObject index: index [
	^ (self host: anObject) index: index
]

{ #category : #comparing }
SpBasicIndexedEyeElement >> = anObject [
	^ super = anObject and: [ index = anObject index ]
]

{ #category : #accessing }
SpBasicIndexedEyeElement >> accessorCode [
	^ '(self basicAt: ', self index asString, ')'
]

{ #category : #comparing }
SpBasicIndexedEyeElement >> hash [
	^ index hash
]

{ #category : #accessing }
SpBasicIndexedEyeElement >> index [
	^ index
]

{ #category : #accessing }
SpBasicIndexedEyeElement >> index: anObject [
	index := anObject
]

{ #category : #accessing }
SpBasicIndexedEyeElement >> label [
	^ self index printString
]

{ #category : #action }
SpBasicIndexedEyeElement >> save: aValue [
	self host basicAt: self index put: aValue
]

{ #category : #accessing }
SpBasicIndexedEyeElement >> value [
	^ self host basicAt: self index
]
