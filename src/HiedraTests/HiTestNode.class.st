"
I am a node in Hiedra tests and examples.
"
Class {
	#name : #HiTestNode,
	#superclass : #Object,
	#instVars : [
		'content',
		'parents',
		'owner',
		'origin'
	],
	#category : #HiedraTests
}

{ #category : #'instance creation' }
HiTestNode class >> content: anObject [
	^ self basicNew
		initializeWithContent: anObject;
		yourself
]

{ #category : #'instance creation' }
HiTestNode class >> content: anObject parents: aCollection [
	^ (self content: anObject)
		parents: aCollection;
		yourself
]

{ #category : #accessing }
HiTestNode >> content [
	^ content
]

{ #category : #initialization }
HiTestNode >> initializeWithContent: anObject [
	self initialize.
	content := anObject.
	parents := #().
]

{ #category : #accessing }
HiTestNode >> origin [
	^ origin
]

{ #category : #accessing }
HiTestNode >> origin: aHiNode [ 
	origin := aHiNode
]

{ #category : #accessing }
HiTestNode >> parents [
	
	^ parents
]

{ #category : #accessing }
HiTestNode >> parents: anObject [
	
	parents := anObject
]

{ #category : #printing }
HiTestNode >> printOn: aStream [ 

	super printOn: aStream.	
	
	aStream 
		nextPut: $(;
		print: content;
		nextPutAll: '->';
		print: parents asArray;
		nextPut: $).
]
