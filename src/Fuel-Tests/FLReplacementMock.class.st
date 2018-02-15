Class {
	#name : #FLReplacementMock,
	#superclass : #Object,
	#instVars : [
		'ignoreMe'
	],
	#category : #'Fuel-Tests-Mocks'
}

{ #category : #comparing }
FLReplacementMock >> = anObject [
	^ self class = anObject class
]

{ #category : #accessing }
FLReplacementMock >> dontIgnoreMe [
	ignoreMe := false
]

{ #category : #accessing }
FLReplacementMock >> fuelReplacement [
	^ ignoreMe
		ifTrue: [ nil ]
		ifFalse: [ self ]
]

{ #category : #accessing }
FLReplacementMock >> ignoreMe [
	^ ignoreMe := true
]
