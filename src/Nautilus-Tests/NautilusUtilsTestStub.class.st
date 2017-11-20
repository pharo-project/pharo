"
This is just a stub class with a stub test, NautilusUtilsStubTest, so that NautilusUtilsTest can test method-to-test method mapping.
"
Class {
	#name : #NautilusUtilsTestStub,
	#superclass : #Object,
	#instVars : [
		'one',
		'two'
	],
	#category : #Nautilus-Tests
}

{ #category : #'instance creation' }
NautilusUtilsTestStub class >> one: aFirstObject two: aSecondObject [
	^ self new
		one: aFirstObject
		two: aSecondObject
]

{ #category : #'instance creation' }
NautilusUtilsTestStub class >> untestedOne: aFirstObject two: aSecondObject [
	^ self new
		one: aFirstObject;
		two: aSecondObject
]

{ #category : #accessing }
NautilusUtilsTestStub >> one [
	^ one
]

{ #category : #accessing }
NautilusUtilsTestStub >> one: anObject [
	one := anObject
]

{ #category : #accessing }
NautilusUtilsTestStub >> one: aFirstObject two: aSecondObject [
	self
		one: aFirstObject;
		two: aSecondObject
]

{ #category : #accessing }
NautilusUtilsTestStub >> oneTwo: anObject [
	self two: anObject
]

{ #category : #accessing }
NautilusUtilsTestStub >> two [
	^ two
]

{ #category : #accessing }
NautilusUtilsTestStub >> two: anObject [
	two := anObject
]

{ #category : #accessing }
NautilusUtilsTestStub >> untestedOne [
	^ self one
]

{ #category : #accessing }
NautilusUtilsTestStub >> untestedOne: anObject [
	self one: anObject
]

{ #category : #accessing }
NautilusUtilsTestStub >> untestedOne: aFirstObject two: aSecondObject [
	self
		one: aFirstObject;
		two: aSecondObject
]

{ #category : #accessing }
NautilusUtilsTestStub >> untestedOneTwo: anObject [
	self two: anObject
]

{ #category : #accessing }
NautilusUtilsTestStub >> untestedTwo [
	^ self two
]

{ #category : #accessing }
NautilusUtilsTestStub >> untestedTwo: anObject [
	self two: anObject
]
