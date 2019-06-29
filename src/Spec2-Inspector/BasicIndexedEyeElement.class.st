"
I am an eye element for indexable fields. (basicAt:)
"
Class {
	#name : #BasicIndexedEyeElement,
	#superclass : #AbstractEyeElement,
	#instVars : [
		'index'
	],
	#category : #'Spec2-Inspector-Element'
}

{ #category : #'instance creation' }
BasicIndexedEyeElement class >> host: anObject index: index [
	^ (self host: anObject) index: index
]

{ #category : #comparing }
BasicIndexedEyeElement >> = anObject [
	^ super = anObject and: [ index = anObject index ]
]

{ #category : #accessing }
BasicIndexedEyeElement >> accessorCode [
	^ '(self basicAt: ', self index asString, ')'
]

{ #category : #comparing }
BasicIndexedEyeElement >> hash [
	^ index hash
]

{ #category : #accessing }
BasicIndexedEyeElement >> index [
	^ index
]

{ #category : #accessing }
BasicIndexedEyeElement >> index: anObject [
	index := anObject
]

{ #category : #accessing }
BasicIndexedEyeElement >> label [
	^ self index printString
]

{ #category : #action }
BasicIndexedEyeElement >> save: aValue [
	self host basicAt: self index put: aValue
]

{ #category : #accessing }
BasicIndexedEyeElement >> value [
	^ self host basicAt: self index
]
