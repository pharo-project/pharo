"
I represent the absense of a type declaration.
I delegate my resolution to the resolver with a double dispatch so it can choose how to resolve non-declared types.
A resolver (FFICallout) has two main resolution strategies: 
- strict does not accept resolving undefined types
- non strict is a backwards compatibility mode that tries to infer the type if possible (but can go wrong and should be used with care)
"
Class {
	#name : #FFIUndefinedTypeDeclaration,
	#superclass : #Object,
	#category : #'UnifiedFFI-Arguments'
}

{ #category : #testing }
FFIUndefinedTypeDeclaration >> isUndefined [

	^ true
]

{ #category : #resolution }
FFIUndefinedTypeDeclaration >> resolveUsing: aResolver forArgument: aFFIFunctionArgument [
	
	^ aResolver resolveUntypedArgument: aFFIFunctionArgument
]
