"
I am ZnByteArrayEntity, a concrete HTTP Entity based on a ByteArray.
It is used to hold binary (non textual) data.
I am a ZnEntity.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnByteArrayEntity,
	#superclass : #ZnEntity,
	#instVars : [
		'bytes'
	],
	#category : #'Zinc-HTTP-Core'
}

{ #category : #'instance creation' }
ZnByteArrayEntity class >> bytes: byteArray [
	^ (self type: ZnMimeType applicationOctetStream length: byteArray size)
		bytes: byteArray;
		yourself 
]

{ #category : #testing }
ZnByteArrayEntity class >> designatedMimeType [
	"I have no designated mime type, I can handle all binary content.
	See #concreteSubclassForType: "
	
	^ nil
]

{ #category : #testing }
ZnByteArrayEntity class >> matches: mimeType [
	"I have no designated mime type, I can handle all binary content.
	See #concreteSubclassForType: "
	
	^ false
]

{ #category : #comparing }
ZnByteArrayEntity >> = other [
	^ super = other and: [ self bytes = other bytes ]
]

{ #category : #accessing }
ZnByteArrayEntity >> bytes [
	^ bytes
]

{ #category : #accessing }
ZnByteArrayEntity >> bytes: anObject [
	bytes := anObject
]

{ #category : #accessing }
ZnByteArrayEntity >> contents [
	^ self bytes
]

{ #category : #comparing }
ZnByteArrayEntity >> hash [
	^ super hash bitXor: self bytes hash
]

{ #category : #testing }
ZnByteArrayEntity >> isEmpty [ 
	^ self bytes isNil or: [ self bytes isEmpty ]
]

{ #category : #printing }
ZnByteArrayEntity >> printContentsOn: stream [
	super printContentsOn: stream.
	self bytes notNil ifTrue: [ 
		stream space; print: self bytes ]
]

{ #category : #'initialize-release' }
ZnByteArrayEntity >> readFrom: stream [
	self contentLength isNil
		ifTrue: [ 
			self bytes: (ZnUtils readUpToEnd: stream limit: ZnMaximumEntitySize value).
			self contentLength: self bytes size ]
		ifFalse: [ | byteArray |
			self contentLength > ZnMaximumEntitySize value
				ifTrue: [ ZnEntityTooLarge signal ].
			byteArray := ByteArray ofSize: self contentLength.
			self contentLength > ZnUtils streamingBufferSize 
				ifTrue: [
					ZnUtils streamFrom: stream to: byteArray writeStream size: self contentLength ]
				ifFalse: [
					stream next: self contentLength into: byteArray ]. 
			self bytes: byteArray ]
]

{ #category : #accessing }
ZnByteArrayEntity >> readStream [
	^ self bytes readStream
]

{ #category : #writing }
ZnByteArrayEntity >> writeOn: stream [
	ZnUtils nextPutAll: self bytes on: stream
]
