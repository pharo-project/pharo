"
I am example of class which is covered by tests using Tests sufix
 
Internal Representation and Key Implementation Points.

    Instance Variables
	one:		<Object>
	two:		<Object>
"
Class {
	#name : #ClyTestedClassMock,
	#superclass : #Object,
	#instVars : [
		'two',
		'one'
	],
	#category : #'Calypso-SystemPlugins-SUnit-Queries-Tests'
}

{ #category : #protocol }
ClyTestedClassMock class >> one: aFirstObject two: aSecondObject [
	^ self new
		one: aFirstObject
		two: aSecondObject
]

{ #category : #protocol }
ClyTestedClassMock class >> untestedOne: aFirstObject two: aSecondObject [
	^ self new
		one: aFirstObject;
		two: aSecondObject
]

{ #category : #accessing }
ClyTestedClassMock >> one [
	^ one
]

{ #category : #accessing }
ClyTestedClassMock >> one: anObject [
	one := anObject 
]

{ #category : #accessing }
ClyTestedClassMock >> one: aFirstObject two: aSecondObject [
	self
		one: aFirstObject;
		two: aSecondObject
]

{ #category : #accessing }
ClyTestedClassMock >> oneTwo: anObject [
	self two: anObject
]

{ #category : #accessing }
ClyTestedClassMock >> two [
	^ two 
]

{ #category : #accessing }
ClyTestedClassMock >> two: anObject [
	two := anObject
]

{ #category : #accessing }
ClyTestedClassMock >> untestedOne [
	^ self one
]

{ #category : #accessing }
ClyTestedClassMock >> untestedOne: anObject [
	self one: anObject
]

{ #category : #accessing }
ClyTestedClassMock >> untestedOne: aFirstObject two: aSecondObject [
	self
		one: aFirstObject;
		two: aSecondObject
]

{ #category : #accessing }
ClyTestedClassMock >> untestedOneTwo: anObject [
	self two: anObject
]

{ #category : #accessing }
ClyTestedClassMock >> untestedTwo [
	^ self two
]

{ #category : #accessing }
ClyTestedClassMock >> untestedTwo: anObject [
	self two: anObject
]
