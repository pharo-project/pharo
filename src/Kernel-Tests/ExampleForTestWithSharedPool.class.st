"
I am class for testing
"
Class {
	#name : #ExampleForTestWithSharedPool,
	#superclass : #ArrayedCollection,
	#instVars : [
		'aSlot',
		'anotherSlot'
	],
	#pools : [
		'ChronologyConstants'
	],
	#category : #'Kernel-Tests-Classes'
}

{ #category : #'as yet unclassified' }
ExampleForTestWithSharedPool >> accessVariables [

	aSlot := anotherSlot := 33
]
