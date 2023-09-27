"
I am a mock used to simlify testing.
	
A mock is an object that simulates the behavior of a real object or component in a controlled manner. It is used in unit testing to isolate the code being tested from its dependencies, allowing the code to be tested in isolation.
"
Class {
	#name : 'MCMockClassF',
	#superclass : 'Object',
	#classVars : [
		'Foo'
	],
	#category : 'MonticelloMocks',
	#package : 'MonticelloMocks'
}

{ #category : 'initialization' }
MCMockClassF >> initialize [
		<ignoreUnusedClassVariables: #( Foo )>
		super initialize.
]
