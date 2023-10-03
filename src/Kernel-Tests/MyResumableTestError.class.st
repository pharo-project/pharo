"
A resumable error for tests
"
Class {
	#name : 'MyResumableTestError',
	#superclass : 'Error',
	#category : 'Kernel-Tests-Exception',
	#package : 'Kernel-Tests',
	#tag : 'Exception'
}

{ #category : 'private - testing' }
MyResumableTestError >> isResumable [

	^ true
]
