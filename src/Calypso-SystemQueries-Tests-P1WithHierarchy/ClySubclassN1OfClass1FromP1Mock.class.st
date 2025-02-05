"
I am a mock used to simlify testing.
	
A mock is an object that simulates the behavior of a real object or component in a controlled manner. It is used in unit testing to isolate the code being tested from its dependencies, allowing the code to be tested in isolation.
"
Class {
	#name : 'ClySubclassN1OfClass1FromP1Mock',
	#superclass : 'ClyClass1FromP1Mock',
	#instVars : [
		'subclassSameVar',
		'subclassN1Var'
	],
	#category : 'Calypso-SystemQueries-Tests-P1WithHierarchy',
	#package : 'Calypso-SystemQueries-Tests-P1WithHierarchy'
}

{ #category : 'accessing' }
ClySubclassN1OfClass1FromP1Mock class >> classSideMethodFromSubclassN1 [
]

{ #category : 'accessing' }
ClySubclassN1OfClass1FromP1Mock >> noTagMethod [
]

{ #category : 'accessing' }
ClySubclassN1OfClass1FromP1Mock >> subclassN1Var [

	^ subclassN1Var
]

{ #category : 'accessing' }
ClySubclassN1OfClass1FromP1Mock >> subclassSameVar [

	^ subclassSameVar
]

{ #category : 'tag1' }
ClySubclassN1OfClass1FromP1Mock >> tag1Method1 [

	"This method is different that its superclass it does nothing because like that 
	it does not get into the method is duplicated from the superclass."
	
	self printOn: ''


]

{ #category : 'tag1' }
ClySubclassN1OfClass1FromP1Mock >> tag1Method2 [
]

{ #category : 'tag2' }
ClySubclassN1OfClass1FromP1Mock >> tag2Method1 [
]
