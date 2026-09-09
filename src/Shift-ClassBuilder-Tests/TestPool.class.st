"
A class to test shared pools in the tests of Shift-Class-Builder
"
Class {
	#name : 'TestPool',
	#superclass : 'SharedPool',
	#classVars : [
		'A',
		'B',
		'C'
	],
	#category : 'Shift-ClassBuilder-Tests-Mock',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'Mock'
}

{ #category : 'initialization' }
TestPool >> initialize [
    <ignoreUnusedClassVariables: #( A B C )>
    super initialize.
]
