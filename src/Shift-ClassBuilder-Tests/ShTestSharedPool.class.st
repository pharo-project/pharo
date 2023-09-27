"
I am a test class.
"
Class {
	#name : 'ShTestSharedPool',
	#superclass : 'SharedPool',
	#classVars : [
		'FooValue1'
	],
	#category : 'Shift-ClassBuilder-Tests-TestClasses',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'TestClasses'
}

{ #category : 'initialization' }
ShTestSharedPool >> initialize [
		<ignoreUnusedClassVariables: #( FooValue1 )>
		super initialize.
]
