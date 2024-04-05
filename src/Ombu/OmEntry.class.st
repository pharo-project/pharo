"
I am a container for recording objects into a store. I also have tags for custom meta-information.
"
Class {
	#name : 'OmEntry',
	#superclass : 'Object',
	#instVars : [
		'tags',
		'content'
	],
	#category : 'Ombu-Entries',
	#package : 'Ombu',
	#tag : 'Entries'
}

{ #category : 'instance creation' }
OmEntry class >> content: anObject [
	^ self basicNew
		initializeWithContent: anObject;
		yourself
]

{ #category : 'comparing' }
OmEntry >> = other [
	^ other species = self species and: [
		other content = self content ]
]

{ #category : 'accessing' }
OmEntry >> content [
	^ content
]

{ #category : 'accessing' }
OmEntry >> content: anObject [
	content := anObject
]

{ #category : 'accessing' }
OmEntry >> fileOutEntryOn: aWriteStream [ 
	"Write the receiver's contents to aWriteStream"

	self content fileOutEntryOn: aWriteStream.
]

{ #category : 'comparing' }
OmEntry >> hash [
	^ self species hash bitXor: self content hash
]

{ #category : 'initialization' }
OmEntry >> initializeWithContent: anObject [
	self initialize.
	content := anObject.
	tags := Dictionary new
]

{ #category : 'testing' }
OmEntry >> isEmpty [
	^ false
]

{ #category : 'testing' }
OmEntry >> matches: aString [ 
	"Answer <true> if the receiver is a code change and its contents name matches aString"
	
	| epEvent |
	
	epEvent := self content.
	^ epEvent isCodeChange and: [ epEvent matches: aString ]
]

{ #category : 'printing' }
OmEntry >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	content printOn: aStream.
	aStream nextPut: $)
]

{ #category : 'accessing' }
OmEntry >> tagAt: aKey [

	^ self tags at: aKey
]

{ #category : 'accessing' }
OmEntry >> tags [
	^ tags
]
