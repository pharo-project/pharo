"
I'm a abstract type for ""referenced"" types. 
See ==FFIExternalReference== for details.
"
Class {
	#name : #FFIExternalReferenceType,
	#superclass : #FFIExternalType,
	#instVars : [
		'objectClass'
	],
	#category : #'UnifiedFFI-Types'
}

{ #category : #accessing }
FFIExternalReferenceType class >> externalType [
	^ ExternalType void asPointerType
]

{ #category : #private }
FFIExternalReferenceType class >> naturalPointerArity [
	^ 1
]

{ #category : #'instance creation' }
FFIExternalReferenceType class >> objectClass: aClass [
	^ self new objectClass: aClass
]

{ #category : #comparing }
FFIExternalReferenceType >> = anExternalReferenceType [

	^ (anExternalReferenceType isKindOf: self species)
		and: [ objectClass = anExternalReferenceType objectClass ]
]

{ #category : #'emitting code' }
FFIExternalReferenceType >> basicEmitArgument: aBuilder context: aContext [
 	self loader
		emitArgument: aBuilder
		context: aContext
		objectClass: self objectClass
		instVarName: self instanceVariableName
]

{ #category : #'emitting code' }
FFIExternalReferenceType >> basicEmitArgument: aBuilder context: aContext inCallout: aCallout [
 	self loader
		emitArgument: aBuilder
		context: aContext
		objectClass: self objectClass
		instVarName: self instanceVariableName
]

{ #category : #private }
FFIExternalReferenceType >> basicHandle: aHandle at: index [
	^ self objectClass fromHandle: (aHandle pointerAt: index)
]

{ #category : #private }
FFIExternalReferenceType >> basicHandle: aHandle at: index put: value [
	value getHandle isExternalAddress
		ifFalse: [ self error: 'value needs to be an ExternalAddress' ].
	^ aHandle pointerAt: index put: value getHandle
]

{ #category : #accessing }
FFIExternalReferenceType >> defaultReturnOnError [

	^ ExternalAddress null
]

{ #category : #'emitting code' }
FFIExternalReferenceType >> emitReturnArgument: builder context: aContext [
	self loader
		emitPointerArityUnpack: builder
		type: self
		context: aContext
		objectClass: self objectClass
		instVarName: self instanceVariableName
]

{ #category : #accessing }
FFIExternalReferenceType >> externalTypeAlignment [
	"this is always a pointer"
	^ self pointerAlignment
]

{ #category : #accessing }
FFIExternalReferenceType >> externalTypeSize [
	"this is always a pointer"
	^ self pointerSize
]

{ #category : #comparing }
FFIExternalReferenceType >> hash [

	^ objectClass hash
]

{ #category : #accessing }
FFIExternalReferenceType >> instanceVariableName [
	^ self subclassResponsibility
]

{ #category : #testing }
FFIExternalReferenceType >> needsArityUnpacking [
	"Referenced types by default can be ''unpacked'' if it needs also to be rolled"
	^ self needsArityPacking
]

{ #category : #accessing }
FFIExternalReferenceType >> objectClass [
	^ objectClass
]

{ #category : #accessing }
FFIExternalReferenceType >> objectClass: aClass [
	objectClass := aClass
]

{ #category : #initialization }
FFIExternalReferenceType >> prepareAsSelfFromCalloutDeclaration [
]

{ #category : #printing }
FFIExternalReferenceType >> printOn: aStream [
	aStream << self className << '(' << objectClass name << ')'
]

{ #category : #'stack parameter classification' }
FFIExternalReferenceType >> stackValueParameterClass [
	^ #integer
]
