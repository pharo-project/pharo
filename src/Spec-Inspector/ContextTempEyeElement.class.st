"
I am an eye element for temporaries in context
"
Class {
	#name : #ContextTempEyeElement,
	#superclass : #AbstractEyeElement,
	#instVars : [
		'tempName',
		'tempIndex'
	],
	#category : #'Spec-Inspector-Element'
}

{ #category : #'instance creation' }
ContextTempEyeElement class >> host: anObject tempName: aString tempIndex: int [
	^ (self host: anObject) 
		tempName: aString;
		tempIndex: int;
		yourself
]

{ #category : #accessing }
ContextTempEyeElement >> accessorCode [
	^ '(self namedTempAt: ', self tempIndex printString, ')'
]

{ #category : #accessing }
ContextTempEyeElement >> label [
	^ self tempName
]

{ #category : #accessing }
ContextTempEyeElement >> save: aValue [
	self host namedTempAt: self tempIndex put: aValue
]

{ #category : #accessing }
ContextTempEyeElement >> tempIndex [
	^ tempIndex
]

{ #category : #accessing }
ContextTempEyeElement >> tempIndex: anObject [
	tempIndex := anObject
]

{ #category : #accessing }
ContextTempEyeElement >> tempName [
	^ tempName
]

{ #category : #accessing }
ContextTempEyeElement >> tempName: anObject [
	tempName := anObject
]

{ #category : #accessing }
ContextTempEyeElement >> value [
	^ self host tempNamed: self tempName
]
