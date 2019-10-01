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
