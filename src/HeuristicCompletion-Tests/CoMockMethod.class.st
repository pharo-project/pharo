"
I am a mock used to simlify testing.
	
A mock is an object that simulates the behavior of a real object or component in a controlled manner. It is used in unit testing to isolate the code being tested from its dependencies, allowing the code to be tested in isolation.
"
Class {
	#name : 'CoMockMethod',
	#superclass : 'Object',
	#instVars : [
		'ast',
		'methodClass'
	],
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'accessing' }
CoMockMethod >> ast [
	^ ast
]

{ #category : 'accessing' }
CoMockMethod >> methodClass [

	^ methodClass
]

{ #category : 'accessing' }
CoMockMethod >> methodClass: aCoMockClass [
	methodClass := aCoMockClass
]

{ #category : 'accessing' }
CoMockMethod >> source: aString [

	ast := OCParser parseMethod: aString
]
