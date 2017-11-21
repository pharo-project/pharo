"
I represent a reference to some variable (a pointer, a memory space, an obscure structure) living in the C heap. 
I'm abstract, my children keep the real references.

A referenced type is  some variable that lives in the C heap and is seen in Pharo as an external  reference (an ==ExternalAddress==) .
"
Class {
	#name : #FFIExternalReference,
	#superclass : #ExternalObject,
	#category : #UnifiedFFI-Objects
}

{ #category : #finalization }
FFIExternalReference class >> finalizeResourceData: handle [
	handle isNull ifTrue: [ ^ self ].
	handle free.
	handle beNull
]

{ #category : #'instance creation' }
FFIExternalReference class >> fromHandle: aHandle [
	^ self new setHandle: aHandle
]

{ #category : #'instance creation' }
FFIExternalReference class >> newPointer [
	^ self fromHandle: ExternalAddress newPointer
]

{ #category : #'external resource management' }
FFIExternalReference >> autoRelease [
	"Note, subclasses should implement #resourceData
	and #finalizeResourceData: on class side"
	FFIExternalResourceManager addResource: self
]

{ #category : #initialization }
FFIExternalReference >> createDefaultHandle [
	^ ExternalAddress new
]

{ #category : #initialization }
FFIExternalReference >> initialize [
	super initialize.
	self setHandle: self createDefaultHandle.
]

{ #category : #printing }
FFIExternalReference >> printOn: aStream [
	super printOn: aStream.
	aStream 
		nextPut: $(;
		print: handle;
		nextPut: $)
]

{ #category : #'external resource management' }
FFIExternalReference >> registerAsExternalResource [
	"Note, subclasses should implement #resourceData
	and #finalizeResourceData: on class side"
	self deprecated: 'Use #autoRelease instead.' on: '2016-01-22' in: #Pharo5.  
	FFIExternalResourceManager addResource: self
]

{ #category : #'external resource management' }
FFIExternalReference >> resourceData [
	^ self getHandle
]
