"
An example class at level 1 (directly below Object) used for testing class hierarchy
"
Class {
	#name : 'ExampleForTest1',
	#superclass : 'Object',
	#instVars : [
		'aSuperclassVariable'
	],
	#category : 'Kernel-Tests-Classes',
	#package : 'Kernel-Tests',
	#tag : 'Classes'
}

{ #category : 'accessing' }
ExampleForTest1 >> aSuperclassVariable [

	^ aSuperclassVariable
]

{ #category : 'accessing' }
ExampleForTest1 >> aSuperclassVariable: anObject [

	aSuperclassVariable := anObject
]
