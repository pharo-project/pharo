"
I am ZnServerSession.
I represent an HTTP session in the context of a ZnServer.

To get a reference to the current session, send #session to a ZnMessage (request or response).

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnServerSession,
	#superclass : #Object,
	#instVars : [
		'id',
		'attributes',
		'created',
		'modified'
	],
	#category : #'Zinc-HTTP-Support'
}

{ #category : #accessing }
ZnServerSession >> age [
	^ DateAndTime now - created
]

{ #category : #accessing }
ZnServerSession >> attributeAt: key [
	^ attributes at: key
]

{ #category : #accessing }
ZnServerSession >> attributeAt: key ifAbsent: block [
	^ attributes at: key ifAbsent: block
]

{ #category : #accessing }
ZnServerSession >> attributeAt: key ifAbsentPut: block [
	^ attributes at: key ifAbsentPut: block
]

{ #category : #accessing }
ZnServerSession >> attributeAt: key put: value [
	^ attributes at: key put: value
]

{ #category : #accessing }
ZnServerSession >> attributeKeys [
	^ attributes keys
]

{ #category : #accessing }
ZnServerSession >> created [
	^ created
]

{ #category : #accessing }
ZnServerSession >> id [
	^ id
]

{ #category : #'initialize-release' }
ZnServerSession >> id: anObject [
	id := anObject
]

{ #category : #accessing }
ZnServerSession >> inactivity [
	^ DateAndTime now - modified 
]

{ #category : #initialization }
ZnServerSession >> initialize [
	super initialize.
	attributes := SmallDictionary new.
	created := modified := DateAndTime now
]

{ #category : #testing }
ZnServerSession >> isValid [
	^ self inactivity < 1 hour
]

{ #category : #accessing }
ZnServerSession >> modified [
	^ modified
]

{ #category : #printing }
ZnServerSession >> printOn: stream [
	super printOn: stream.
	id
		ifNotNil: [ 
			stream
				nextPut: $(;
				print: id;
				nextPut: $) ]
]

{ #category : #accessing }
ZnServerSession >> removeAttribute: key [
	attributes removeKey: key ifAbsent: [ ] 
]

{ #category : #accessing }
ZnServerSession >> touch [
	modified := DateAndTime now
]
