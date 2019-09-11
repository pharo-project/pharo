"
A mock for a singleton
"
Class {
	#name : #FLSingletonMock,
	#superclass : #Object,
	#instVars : [
		'reference'
	],
	#classVars : [
		'Instance'
	],
	#category : #'Fuel-Tests-Core-Mocks'
}

{ #category : #accessing }
FLSingletonMock class >> instance [
	^ Instance ifNil: [ Instance := self basicNew ]
]

{ #category : #'instance creation' }
FLSingletonMock class >> new [
	self error: 'I''m a singleton!'
]

{ #category : #initialization }
FLSingletonMock class >> reset [
	Instance := nil
]

{ #category : #accessing }
FLSingletonMock >> reference [
	^ reference
]

{ #category : #accessing }
FLSingletonMock >> reference: anObject [
	^ reference := anObject
]
