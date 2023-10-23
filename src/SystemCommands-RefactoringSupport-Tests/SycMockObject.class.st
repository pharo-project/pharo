"
I am a mock to emulate a Change
"
Class {
	#name : 'SycMockObject',
	#superclass : 'Object',
	#instVars : [
		'changeClass'
	],
	#category : 'SystemCommands-RefactoringSupport-Tests',
	#package : 'SystemCommands-RefactoringSupport-Tests'
}

{ #category : 'accessing' }
SycMockObject >> changeClass [

	^ changeClass
]

{ #category : 'accessing' }
SycMockObject >> changeClass: anObject [

	changeClass := anObject
]
