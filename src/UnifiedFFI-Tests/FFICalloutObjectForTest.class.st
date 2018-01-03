"
A callout object for testing purposes
"
Class {
	#name : #FFICalloutObjectForTest,
	#superclass : #Object,
	#instVars : [
		'handle'
	],
	#category : #'UnifiedFFI-Tests'
}

{ #category : #converting }
FFICalloutObjectForTest class >> asExternalTypeOn: generator [
	^ FFIExternalObjectType objectClass: self
]

{ #category : #primitives }
FFICalloutObjectForTest class >> primCreate: size [
	self 
		ffiCall: #( FFICalloutObjectForTest malloc( size_t size ) )
		module: LibC	
]

{ #category : #primitives }
FFICalloutObjectForTest class >> primObject: anObject strcpy: aString [
	self
		ffiCall: #( char *strcpy ( FFICalloutObjectForTest anObject, const char *aString ) )
		module: LibC
]

{ #category : #accessing }
FFICalloutObjectForTest >> free [
	handle ifNil: [ ^ self ].
	self primFree
]

{ #category : #accessing }
FFICalloutObjectForTest >> handle [
	^ handle
]

{ #category : #accessing }
FFICalloutObjectForTest >> handle: anObject [
	handle := anObject
]

{ #category : #primitives }
FFICalloutObjectForTest >> primFree [
	^ self 
		ffiCall: #( void free ( self ) )
		module: LibC
]

{ #category : #primitives }
FFICalloutObjectForTest >> primStrcpy: aString [
	^ self 
		ffiCall: #( char *strcpy ( self, const char *aString ) )
		module: LibC
]

{ #category : #primitives }
FFICalloutObjectForTest >> primStrcpy: aString size: n [
	^ self 
		ffiCall: #( char *strncpy ( self, const char *aString, size_t n ) )
		module: LibC
]

{ #category : #accessing }
FFICalloutObjectForTest >> strcpy: aString [
	handle := ByteArray new: aString size + 1.
	^ self primStrcpy: aString.
]
