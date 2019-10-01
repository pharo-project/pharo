"
I resolve ffi call-out arguments in a non-strict way.
This is the backwards compatibility mode and should not be used unless you know what you do.

I try to guess what type to use for a literal.
For example, for a self reference I can guess it by looking at the type of the class of self.
However, some types are problematic. For example, ints and floats can have different size/precisions.
"
Class {
	#name : #FFIInferenceResolutionMode,
	#superclass : #Object,
	#category : #'UnifiedFFI-Callouts'
}

{ #category : #testing }
FFIInferenceResolutionMode >> isStrict [
	
	^ false
]

{ #category : #resolution }
FFIInferenceResolutionMode >> resolveUndeclaredTypeForArgument: aFFIValueArgument withResolver: aResolver [

	(aFFIValueArgument value isNil
		or: [aFFIValueArgument value = 'nil'
			or: [ aFFIValueArgument value = 'NULL' ]])
				ifTrue: [ ^ (aResolver resolveType: #'void *') ].
	(#(true false) includes: aFFIValueArgument value) 
		ifTrue: [ ^ aResolver resolveType: #bool ].
	aFFIValueArgument value isInteger
		ifTrue: [ ^ (aFFIValueArgument value >=0 
			ifTrue: [ aResolver resolveType: #uint32 ]
			ifFalse: [ aResolver resolveType: #int32 ]) ].
	(aFFIValueArgument value == #self) ifTrue: [ 
		^ (aResolver requestor asExternalTypeOn: aResolver) 
			prepareAsSelfFromCalloutDeclaration ].
	
	^ aFFIValueArgument value isSymbol
		ifTrue: [ 
			"This is actually a class variable with a value..."
			aResolver resolveType: aFFIValueArgument value ]
		ifFalse: [ aResolver unsupportedUntypedLiteral: aFFIValueArgument value ]
]
