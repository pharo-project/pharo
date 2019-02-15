"
I am a container for recording objects into a store. I also have tags for custom meta-information.
"
Class {
	#name : #OmEntry,
	#superclass : #Object,
	#instVars : [
		'tags',
		'content'
	],
	#category : #'Ombu-Entries'
}

{ #category : #'instance creation' }
OmEntry class >> content: anObject [
	^ self basicNew 
		initializeWithContent: anObject;
		yourself
]

{ #category : #comparing }
OmEntry >> = other [
	^ other species = self species and: [ 
		other content = self content ]
]

{ #category : #accessing }
OmEntry >> content [
	^ content
]

{ #category : #accessing }
OmEntry >> content: anObject [ 
	content := anObject
]

{ #category : #comparing }
OmEntry >> hash [
	^ self species hash bitXor: self content hash
]

{ #category : #initialization }
OmEntry >> initializeWithContent: anObject [
	self initialize.
	content := anObject.
	tags := Dictionary new.
]

{ #category : #testing }
OmEntry >> isEmpty [
	^ false
]

{ #category : #printing }
OmEntry >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	content printOn: aStream.
	aStream nextPut: $).
]

{ #category : #accessing }
OmEntry >> tagAt: aKey [

	^ self tags at: aKey 
]

{ #category : #accessing }
OmEntry >> tags [
	^ tags
]
