"
String type.
Represented as char* on C side.
"
Class {
	#name : #FFIExternalString,
	#superclass : #FFIExternalType,
	#category : #UnifiedFFI-Types
}

{ #category : #accessing }
FFIExternalString class >> externalType [
	^ ExternalType string
]

{ #category : #accessing }
FFIExternalString class >> externalTypeAlignment [
	^ self pointerSize
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
FFIExternalString >> externalTypeSize [
	^ self pointerSize "i am live and die as a pointer (a char*)"
]

{ #category : #'stack parameter classification' }
FFIExternalString >> stackValueParameterClass [
	^ #integer
]
