"
String type.
Represented as char* on C side.
"
Class {
	#name : #FFIExternalString,
	#superclass : #FFIExternalType,
	#category : #'UnifiedFFI-Types'
}

{ #category : #accessing }
FFIExternalString class >> externalType [
	^ ExternalType string
]

{ #category : #accessing }
FFIExternalString class >> externalTypeAlignment [
	^ self pointerSize
]

{ #category : #'emitting code' }
FFIExternalString >> basicEmitArgument: aBuilder context: aContext inCallout: aCallout [

	self loader 
		emitArgument: aBuilder 
		context: aContext
]

{ #category : #private }
FFIExternalString >> basicHandle: aHandle at: index [
	| data |
	data := ExternalData 
		fromHandle: (aHandle pointerAt: index) 
		type: ExternalType string.
	data isNull ifTrue: [ ^ nil ].
	^ data fromCString
]

{ #category : #private }
FFIExternalString >> basicHandle: aHandle at: index put: value [
	self error: 'Not sure I want to do this.'
]

{ #category : #accessing }
FFIExternalString >> defaultReturnOnError [

	^ ''
]

{ #category : #accessing }
FFIExternalString >> externalTypeSize [
	^ self pointerSize "i am live and die as a pointer (a char*)"
]

{ #category : #callbacks }
FFIExternalString >> extractFromCallbackOn: aFFICallbackArgumentReader [ 
	
	aFFICallbackArgumentReader extractExternalString: self
]

{ #category : #'stack parameter classification' }
FFIExternalString >> stackValueParameterClass [
	^ #integer
]
