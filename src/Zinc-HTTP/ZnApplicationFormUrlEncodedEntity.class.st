"
I am ZnApplicationFormUrlEncodedEntity, a concrete HTTP Entity 
holding a set of fields (key value pairs) that is form URL encoded.
I am a ZnEntity.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnApplicationFormUrlEncodedEntity,
	#superclass : #ZnEntity,
	#instVars : [
		'fields',
		'representation'
	],
	#category : 'Zinc-HTTP-Core'
}

{ #category : #testing }
ZnApplicationFormUrlEncodedEntity class >> designatedMimeType [
	^ ZnMimeType applicationFormUrlEncoded 
]

{ #category : #'instance creation' }
ZnApplicationFormUrlEncodedEntity class >> withAll: keyedCollection [
	^ self new
		addAll: keyedCollection;
		yourself 
]

{ #category : #accessing }
ZnApplicationFormUrlEncodedEntity >> add: association [
	self at: association key put: association value
]

{ #category : #accessing }
ZnApplicationFormUrlEncodedEntity >> addAll: keyedCollection [
	"Note that we use #addAllMulti:"

	self invalidateRepresentation.
	self fields addAllMulti: keyedCollection.
	^ keyedCollection 
]

{ #category : #accessing }
ZnApplicationFormUrlEncodedEntity >> at: key [
	^ self fields at: key
]

{ #category : #accessing }
ZnApplicationFormUrlEncodedEntity >> at: key add: value [
	self invalidateRepresentation.
	^ self fields at: key add: value
]

{ #category : #accessing }
ZnApplicationFormUrlEncodedEntity >> at: key ifAbsent: block [
	^ self fields at: key ifAbsent: block
]

{ #category : #accessing }
ZnApplicationFormUrlEncodedEntity >> at: key put: value [
	self invalidateRepresentation.
	^ self fields at: key put: value
]

{ #category : #private }
ZnApplicationFormUrlEncodedEntity >> computeRepresentation [
	representation := String streamContents: [ :stream | self writeRepresentationOn: stream ].
	contentLength := representation size
]

{ #category : #accessing }
ZnApplicationFormUrlEncodedEntity >> contentLength [
	contentLength isNil ifTrue: [ self computeRepresentation ].
	^ contentLength 
]

{ #category : #accessing }
ZnApplicationFormUrlEncodedEntity >> contents [
	^ self fields
]

{ #category : #accessing }
ZnApplicationFormUrlEncodedEntity >> fields [
	^ fields
]

{ #category : #enumerating }
ZnApplicationFormUrlEncodedEntity >> fieldsDo: block [
	self fields keysAndValuesDo: block
]

{ #category : #testing }
ZnApplicationFormUrlEncodedEntity >> includesField: field [
	^ self fields includesKey: field
]

{ #category : #initialization }
ZnApplicationFormUrlEncodedEntity >> initialize [
	super initialize.
	fields := ZnMultiValueDictionary new.
	self contentType: self class designatedMimeType 
]

{ #category : #private }
ZnApplicationFormUrlEncodedEntity >> invalidateRepresentation [
	representation := nil.
	contentLength := nil
]

{ #category : #testing }
ZnApplicationFormUrlEncodedEntity >> isEmpty [
	^ self fields isEmpty
]

{ #category : #printing }
ZnApplicationFormUrlEncodedEntity >> printContentsOn: stream [
	super printContentsOn: stream.
	self isEmpty
		ifTrue: [ ^ self ].
	stream space.
	fields printElementsOn: stream
]

{ #category : #initialize-release }
ZnApplicationFormUrlEncodedEntity >> readFrom: stream [
	"We parse from stream, limited to content length if available."
	
	| readStream |
	representation := nil.
	contentLength isNil
		ifTrue: [ readStream := ZnCharacterReadStream on: stream encoding: 'ascii' ]
		ifFalse: [ (readStream := ZnLimitedReadStream on: stream limit: contentLength) ascii ].
	fields := ZnResourceMetaUtils parseQueryFrom: readStream
]

{ #category : #writing }
ZnApplicationFormUrlEncodedEntity >> writeOn: stream [
	"In most use cases, contentLength will have been asked first so that a representation is cached"
	
	representation isNil ifTrue: [ self computeRepresentation ].
	ZnUtils nextPutAll: representation on: stream
]

{ #category : #writing }
ZnApplicationFormUrlEncodedEntity >> writeRepresentationOn: stream [
	self contentType charSet
		ifNotNil: [:charSet|
			ZnResourceMetaUtils
				writeQueryFields: self fields 
				withEncoder: (ZnCharacterEncoder newForEncoding: charSet)
				on: stream] 
		ifNil: [ 
			ZnResourceMetaUtils 
				writeQueryFields: self fields 
				on: stream ]
]
