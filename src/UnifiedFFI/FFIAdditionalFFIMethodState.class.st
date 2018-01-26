"
I am a FFIAdditionalFFIMethodState. I am an extenstion of the AdditionalMethodState that store the arguments names to be used for FFI when the sources are not loaded or unloaded.

See also FFICompilerPlugin and FDBDecompiler>>createNArgs:

argsNames: Collection of Strings
"
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
FFIAdditionalFFIMethodState >> decompiledArgumentNameAt: aNumber [
	^ self argsNames at: aNumber
]

{ #category : #testing }
FFIAdditionalFFIMethodState >> isFFIMethodState [
	^ true
]
