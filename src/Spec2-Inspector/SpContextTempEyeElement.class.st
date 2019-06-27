"
I am an eye element for temporaries in context
"
Class {
	#name : #SpContextTempEyeElement,
	#superclass : #SpAbstractEyeElement,
	#instVars : [
		'tempName',
		'tempIndex'
	],
	#category : #'Spec2-Inspector-Element'
}

{ #category : #'instance creation' }
SpContextTempEyeElement class >> host: anObject tempName: aString tempIndex: int [
	^ (self host: anObject) 
		tempName: aString;
		tempIndex: int;
		yourself
]

{ #category : #accessing }
SpContextTempEyeElement >> accessorCode [
	^ '(self namedTempAt: ', self tempIndex printString, ')'
]

{ #category : #accessing }
SpContextTempEyeElement >> label [
	^ self tempName
]

{ #category : #accessing }
SpContextTempEyeElement >> save: aValue [
	self host namedTempAt: self tempIndex put: aValue
]

{ #category : #accessing }
SpContextTempEyeElement >> tempIndex [
	^ tempIndex
]

{ #category : #accessing }
SpContextTempEyeElement >> tempIndex: anObject [
	tempIndex := anObject
]

{ #category : #accessing }
SpContextTempEyeElement >> tempName [
	^ tempName
]

{ #category : #accessing }
SpContextTempEyeElement >> tempName: anObject [
	tempName := anObject
]

{ #category : #accessing }
SpContextTempEyeElement >> value [
	^ self host tempNamed: self tempName
]
