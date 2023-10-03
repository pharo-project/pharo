"
I am a mock used to simlify testing.
	
A mock is an object that simulates the behavior of a real object or component in a controlled manner. It is used in unit testing to isolate the code being tested from its dependencies, allowing the code to be tested in isolation.
"
Class {
	#name : 'ClySubclassLOfClass2FromP1Mock',
	#superclass : 'ClyClass2FromP1Mock',
	#classVars : [
		'SubclassClassVar'
	],
	#category : 'Calypso-SystemQueries-Tests-P1WithHierarchy',
	#package : 'Calypso-SystemQueries-Tests-P1WithHierarchy'
}

{ #category : 'accessing' }
ClySubclassLOfClass2FromP1Mock >> subclassClassVar1Reader [
	^ClassVar1
]

{ #category : 'accessing' }
ClySubclassLOfClass2FromP1Mock >> subclassClassVar1Writer [
	ClassVar1 := #subclassClassVar1Value
]

{ #category : 'accessing' }
ClySubclassLOfClass2FromP1Mock >> subclassClassVarReader [
	^SubclassClassVar
]
