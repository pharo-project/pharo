"
I am a mock used to simlify testing.
	
A mock is an object that simulates the behavior of a real object or component in a controlled manner. It is used in unit testing to isolate the code being tested from its dependencies, allowing the code to be tested in isolation.
"
Class {
	#name : 'ClyClass1FromP1Mock',
	#superclass : 'Object',
	#instVars : [
		'instanceSideVar1',
		'instanceSideVar2'
	],
	#category : 'Calypso-SystemQueries-Tests-P1WithHierarchy',
	#package : 'Calypso-SystemQueries-Tests-P1WithHierarchy'
}

{ #category : 'protocol' }
ClyClass1FromP1Mock class >> classSideMethodFromClass1 [
]

{ #category : 'var accessors' }
ClyClass1FromP1Mock >> instanceSideVar1ReaderMethod [
	^instanceSideVar1
]

{ #category : 'var accessors' }
ClyClass1FromP1Mock >> instanceSideVar1WriterMethod [
	instanceSideVar1 := #var1Value
]

{ #category : 'accessing' }
ClyClass1FromP1Mock >> instanceSideVar2 [
	^ instanceSideVar2
]

{ #category : 'superclassTag1' }
ClyClass1FromP1Mock >> superclassTag1Method [
]

{ #category : 'tag1' }
ClyClass1FromP1Mock >> tag1Method1 [
]
