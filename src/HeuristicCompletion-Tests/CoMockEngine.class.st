"
I am a mock used to simlify testing.
	
A mock is an object that simulates the behavior of a real object or component in a controlled manner. It is used in unit testing to isolate the code being tested from its dependencies, allowing the code to be tested in isolation.
"
Class {
	#name : 'CoMockEngine',
	#superclass : 'Object',
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'accessing' }
CoMockEngine >> completionToken [
	"This is the token used to autocomplete"
	^ 'token'
]

{ #category : 'replacement' }
CoMockEngine >> completionTokenStart [ 
	^ 1
]
