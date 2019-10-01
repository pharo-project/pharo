"
I resolve arguments strictly: if they have no type declaration I throw an exception.
"
Class {
	#name : #FFIStrictResolutionMode,
	#superclass : #Object,
	#category : #'UnifiedFFI-Callouts'
}

{ #category : #resolution }
FFIStrictResolutionMode >> resolveUndeclaredTypeForArgument: aFFIValueArgument withResolver: aResolver [

	aResolver unsupportedUntypedLiteral: aFFIValueArgument value
]
