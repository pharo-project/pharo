"
A class to test shared pools in the tests of Shift-Class-Builder
"
Class {
	#name : 'TestPool2',
	#superclass : 'SharedPool',
	#classVars : [
		'D',
		'E'
	],
	#category : 'Shift-ClassBuilder-Tests-Mock',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'Mock'
}

{ #category : 'initialization' }
TestPool2 >> initialize [
    <ignoreUnusedClassVariables: #( D E )>
    super initialize.
]
