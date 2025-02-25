"
I resolve arguments strictly: if they have no type declaration I throw an exception.
"
Class {
	#name : 'FFIStrictResolutionMode',
	#superclass : 'Object',
	#category : 'UnifiedFFI-Callouts',
	#package : 'UnifiedFFI',
	#tag : 'Callouts'
}

{ #category : 'testing' }
FFIStrictResolutionMode >> isStrict [

	^ true
]

{ #category : 'resolution' }
FFIStrictResolutionMode >> resolveUndeclaredTypeForArgument: aFFIValueArgument withResolver: aResolver [

	"Make strict all declarations except those using self and NULL.
	This is safe because:
	 - We can infer self from the current class
	 - the only implementor of prepareAsSelfFromCalloutDeclaration are those classes representing pointers
	 - NULL can only be of one specific type: void*
	 	 There is no ambiguity.
	"
	
	(#(nil 'nil' 'NULL') includes: aFFIValueArgument value)
		 ifTrue: [ ^ (aResolver resolveType: #'void *') ].
				
	aFFIValueArgument value == #self ifTrue: [
		| externalType |
		externalType := aResolver requestor asExternalTypeOn: aResolver.
		^ externalType prepareAsSelfFromCalloutDeclaration ].

	"Otherwise, this is an untyped literal which is unsupported"
	aResolver unsupportedUntypedLiteral: aFFIValueArgument value
]
