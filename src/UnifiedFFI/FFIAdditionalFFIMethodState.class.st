Class {
	#name : #FFIAdditionalFFIMethodState,
	#superclass : #AdditionalMethodState,
	#type : #variable,
	#instVars : [
		'argsNames'
	],
	#category : #UnifiedFFI
}

{ #category : #accessing }
FFIAdditionalFFIMethodState >> argsNames [
	^ argsNames
]

{ #category : #accessing }
FFIAdditionalFFIMethodState >> argsNames: anObject [
	argsNames := anObject
]

{ #category : #testing }
FFIAdditionalFFIMethodState >> isFFIMethodState [
	^ true
]
