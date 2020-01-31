"
I am a string encoding strategy that sends strings as they are to the callout primitive.
I am used for the legacy and backwards compatibility behavior that just copies the bytes of the ByteString to the C string.
"
Class {
	#name : #FFILegacyStringEncodingStrategy,
	#superclass : #Object,
	#category : #'UnifiedFFI-Strings'
}

{ #category : #emitting }
FFILegacyStringEncodingStrategy >> emitIRForFunctionArgument: aFFICharacterType inIRBuilder: anIRBuilder context: aContext [ 

	aFFICharacterType loader
		emitArgument: anIRBuilder
		context: aContext
]

{ #category : #emitting }
FFILegacyStringEncodingStrategy >> emitIRForFunctionReturn: aFFICharacterType inIRBuilder: aBuilder context: aContext [

	^ aBuilder returnTop
]

{ #category : #testing }
FFILegacyStringEncodingStrategy >> isLegacy [

	^ true
]
