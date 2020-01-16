"
Character type.
Represented as single byte on C side.
"
Class {
	#name : #FFICharacterType,
	#superclass : #FFIExternalType,
	#category : #'UnifiedFFI-Types'
}

{ #category : #accessing }
FFICharacterType class >> externalType [
	^ ExternalType char
]

{ #category : #accessing }
FFICharacterType class >> externalTypeAlignment [
	^ 1
]

{ #category : #accessing }
FFICharacterType class >> externalTypeSize [
	^ 1
]

{ #category : #'emitting code' }
FFICharacterType >> basicEmitArgument: aBuilder context: aContext inCallout: aCallout [

	| isString |
	isString := self pointerArity = 1.
	isString
		ifTrue: [
			aCallout stringEncodingStrategy
				emitIRForFunctionArgument: self inIRBuilder: aBuilder context: aContext ]
		ifFalse: [ super basicEmitArgument: aBuilder context: aContext inCallout: aCallout ]
]

{ #category : #private }
FFICharacterType >> basicHandle: aHandle at: index [
	^ aHandle signedCharAt: index
]

{ #category : #private }
FFICharacterType >> basicHandle: aHandle at: index put: value [
	^ aHandle signedCharAt: index put: value
]

{ #category : #accessing }
FFICharacterType >> defaultReturnOnError [

	^ Character null
]

{ #category : #'emitting code' }
FFICharacterType >> emitReturn: aBuilder resultTempVar: resultVar context: aContext inCallout: aCallout [

	| isString |
	isString := self pointerArity = 1.
	^ isString
		ifTrue: [
			aCallout stringEncodingStrategy
				emitIRForFunctionReturn: self inIRBuilder: aBuilder context: aContext ]
		ifFalse: [ super emitReturn: aBuilder resultTempVar: resultVar context: aContext inCallout: aCallout ]
]

{ #category : #callbacks }
FFICharacterType >> extractFromCallbackOn: aCallbackArgumentReader [ 

	aCallbackArgumentReader extractCharacterType
]

{ #category : #testing }
FFICharacterType >> needsArityPacking [
	">1 because it can be a 'char *', then just roll when is 'char**' or bigger"
	^ self pointerArity > 1
]

{ #category : #testing }
FFICharacterType >> needsArityUnpacking [
	"'char **' or bigger need to be unpacked"
	^ self needsArityPacking
]

{ #category : #'stack parameter classification' }
FFICharacterType >> stackValueParameterClass [
	^ #integer
]
