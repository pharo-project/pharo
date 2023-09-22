"
I am a mock used to simlify testing.
	
A mock is an object that simulates the behavior of a real object or component in a controlled manner. It is used in unit testing to isolate the code being tested from its dependencies, allowing the code to be tested in isolation.
"
Class {
	#name : 'ClySubclassN2OfClass1FromP1Mock',
	#superclass : 'ClyClass1FromP1Mock',
	#instVars : [
		'subclassSameVar',
		'subclassN2Var'
	],
	#category : 'Calypso-SystemQueries-Tests-P1WithHierarchy',
	#package : 'Calypso-SystemQueries-Tests-P1WithHierarchy'
}

{ #category : 'accessing' }
ClySubclassN2OfClass1FromP1Mock class >> classSideMethodFromSubclassN2 [
]

{ #category : 'accessing' }
ClySubclassN2OfClass1FromP1Mock >> subclassN2VarReaderMethod [
	^subclassN2Var
]

{ #category : 'accessing' }
ClySubclassN2OfClass1FromP1Mock >> subclassN2VarWriterMethod [
	subclassN2Var := #subclassN2VarValue
]

{ #category : 'accessing' }
ClySubclassN2OfClass1FromP1Mock >> subclassSameVar [

	^ subclassSameVar
]

{ #category : 'accessing' }
ClySubclassN2OfClass1FromP1Mock >> superclassVar1ReaderMethod [
	^instanceSideVar1
]

{ #category : 'accessing' }
ClySubclassN2OfClass1FromP1Mock >> superclassVar1WriterMethod [
	instanceSideVar1 := #subclassVar1Value
]
