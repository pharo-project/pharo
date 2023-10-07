"
An example class at level 2 (indirectly below Object) used for testing class hierarchy
"
Class {
	#name : 'ExampleForTest11',
	#superclass : 'ExampleForTest1',
	#instVars : [
		'aSubclassVariable'
	],
	#category : 'Kernel-Tests-Classes',
	#package : 'Kernel-Tests',
	#tag : 'Classes'
}

{ #category : 'accessing' }
ExampleForTest11 >> aSubclassVariable [

	^ aSubclassVariable
]

{ #category : 'accessing' }
ExampleForTest11 >> aSubclassVariable: anObject [

	aSubclassVariable := anObject
]
