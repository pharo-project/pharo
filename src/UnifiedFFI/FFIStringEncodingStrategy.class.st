"
I am a string encoding strategy that explicitly encodes a string into bytes before sending it to the callout.
I know how to emit the necessary IR code to push a string argument and encode it.
"
Class {
	#name : #FFIStringEncodingStrategy,
	#superclass : #Object,
	#instVars : [
		'encoding'
	],
	#category : #'UnifiedFFI-Strings'
}

{ #category : #'instance creation' }
FFIStringEncodingStrategy class >> forEncoding: aString [

	^ self new
		encoding: aString;
		yourself
]

{ #category : #emitting }
FFIStringEncodingStrategy >> emitIRForFunctionArgument: aFFICharacterType inIRBuilder: anIRBuilder context: aContext [ 
	
	aFFICharacterType loader emitArgument: anIRBuilder context: aContext.
	anIRBuilder pushLiteral: encoding asSymbol.
	anIRBuilder send: #nullTerminatedEncodeWith:.
]

{ #category : #emitting }
FFIStringEncodingStrategy >> emitIRForFunctionReturn: aFFICharacterType inIRBuilder: anIRBuilder context: aContext [
	
	"The top of the stack should contain a ByteString with the bytes of the returned char*.
	The FFI backend assumes (correctly) that it is a null terminated string, and (incorrectly) that the encoding is the same of the image.
	We should transform the byte string into a byte array and then decode it"
	anIRBuilder send: #asByteArray.
	anIRBuilder pushLiteral: encoding asSymbol.
	anIRBuilder send: #decodeWith:.
	^ anIRBuilder returnTop
]

{ #category : #accessing }
FFIStringEncodingStrategy >> encoding [
	^ encoding
]

{ #category : #accessing }
FFIStringEncodingStrategy >> encoding: aString [ 
	encoding := aString
]
