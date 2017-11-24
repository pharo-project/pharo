"
A resumable error for tests
"
Class {
	#name : #MyResumableTestError,
	#superclass : #Error,
	#category : #'Kernel-Tests-Exception'
}

{ #category : #private }
MyResumableTestError >> isResumable [

	^true
]
