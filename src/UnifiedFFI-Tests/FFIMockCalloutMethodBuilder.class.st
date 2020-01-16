"
I am a callout method builder that creates a mock ExternalLibraryFunction invoking a block instead of a function.
Useful for testing, avoiding the actual external call.
"
Class {
	#name : #FFIMockCalloutMethodBuilder,
	#superclass : #FFICalloutMethodBuilder,
	#instVars : [
		'block'
	],
	#category : #'UnifiedFFI-Tests-Tests'
}

{ #category : #compatibility }
FFIMockCalloutMethodBuilder >> createFFICalloutLiteralFromSpec: functionSpec [

	^ self
]

{ #category : #accessing }
FFIMockCalloutMethodBuilder >> doing: aBlockClosure [ 
	
	block := aBlockClosure
]

{ #category : #'type-resolution' }
FFIMockCalloutMethodBuilder >> ffiBindingOf: aString [ 
	
	^ self class ffiBindingOf: aString
]

{ #category : #invoking }
FFIMockCalloutMethodBuilder >> invokeWithArguments: aCollection [ 
	
	^ block valueWithArguments: aCollection
]

{ #category : #compatibility }
FFIMockCalloutMethodBuilder >> method [
	"To mock the creation of the IR..."
	^ block
]

{ #category : #compatibility }
FFIMockCalloutMethodBuilder >> methodProperties [
	"To mock the creation of the IR..."
	^ nil
]

{ #category : #compatibility }
FFIMockCalloutMethodBuilder >> methodTrailer [

	^ CompiledMethodTrailer empty
]
