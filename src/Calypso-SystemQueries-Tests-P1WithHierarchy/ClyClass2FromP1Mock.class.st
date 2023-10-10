"
I am a mock used to simlify testing.
	
A mock is an object that simulates the behavior of a real object or component in a controlled manner. It is used in unit testing to isolate the code being tested from its dependencies, allowing the code to be tested in isolation.
"
Class {
	#name : 'ClyClass2FromP1Mock',
	#superclass : 'Object',
	#instVars : [
		'instVar'
	],
	#classVars : [
		'ClassVar1',
		'ClassVar2'
	],
	#category : 'Calypso-SystemQueries-Tests-P1WithHierarchy',
	#package : 'Calypso-SystemQueries-Tests-P1WithHierarchy'
}

{ #category : 'accessing' }
ClyClass2FromP1Mock class >> classSideClassVar1Reader [
	^ClassVar1
]

{ #category : 'accessing' }
ClyClass2FromP1Mock class >> classSideClassVar1Writer [
	ClassVar1 := #classVar1Value
]

{ #category : 'accessing' }
ClyClass2FromP1Mock >> instVar [

	 ^ instVar
]

{ #category : 'as yet unclassified' }
ClyClass2FromP1Mock >> instanceSideClassVar1Reader [
	^ClassVar1
]

{ #category : 'accessing' }
ClyClass2FromP1Mock >> instanceSideClassVar1Writer [
	ClassVar1 := #classVar1Value
]

{ #category : 'accessing' }
ClyClass2FromP1Mock >> instanceSideClassVar2Reader [
	^ClassVar2
]
