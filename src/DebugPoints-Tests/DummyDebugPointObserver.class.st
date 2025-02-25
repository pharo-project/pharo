"
Mock for testing debug point observer.
It bypasses the system observer for test purposes.
"
Class {
	#name : 'DummyDebugPointObserver',
	#superclass : 'Object',
	#instVars : [
		'tag'
	],
	#category : 'DebugPoints-Tests-Utils',
	#package : 'DebugPoints-Tests',
	#tag : 'Utils'
}

{ #category : 'accessing' }
DummyDebugPointObserver >> tag [

	^ tag
]

{ #category : 'accessing' }
DummyDebugPointObserver >> tag: aSymbol [

	tag := aSymbol
]

{ #category : 'updating' }
DummyDebugPointObserver >> update: anAspect [

	tag := anAspect	
]
