"
I am a special global variable that is marked as deprecated. 

This allows to keep the global, but to strike throught its name and raise warning when we access it.
"
Class {
	#name : #DeprecatedGlobalVariable,
	#superclass : #GlobalVariable,
	#category : #'Kernel-Variables'
}

{ #category : #testing }
DeprecatedGlobalVariable >> isDeprecated [

	^ true
]
