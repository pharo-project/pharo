"
I represent a comment to Epicea entries  performed in an EpLogBrowser.
"
Class {
	#name : #EpLogEntriesComment,
	#superclass : #EpLogBrowserOperation,
	#instVars : [
		'newComment',
		'oldComment'
	],
	#category : #'Epicea-Model'
}

{ #category : #'instance creation' }
EpLogEntriesComment class >> entryReferences: aCollection oldComment: aString newComment: anotherString [
	^ self basicNew
		initializeWithEntryReferences: aCollection 
		oldComment: aString 
		newComment: anotherString;
		yourself
]

{ #category : #visitor }
EpLogEntriesComment >> accept: aVisitor [ 
	^ aVisitor visitLogCommentModification: self
]

{ #category : #accessing }
EpLogEntriesComment >> comment [
	^ self newComment
]

{ #category : #copying }
EpLogEntriesComment >> copyWithEntryReferences: aCollection [
	^ self class
		entryReferences: aCollection
		oldComment: self oldComment
		newComment: self newComment
]

{ #category : #initialization }
EpLogEntriesComment >> initializeWithEntryReferences: aCollection oldComment: theOldComment newComment: theNewComment [ 
	self initializeWith: aCollection.
	oldComment := theOldComment.
	newComment := theNewComment.

]

{ #category : #testing }
EpLogEntriesComment >> isEpLogEntriesComment [

	^ true
]

{ #category : #accessing }
EpLogEntriesComment >> newComment [
	^ newComment
]

{ #category : #accessing }
EpLogEntriesComment >> oldComment [
	^ oldComment
]
