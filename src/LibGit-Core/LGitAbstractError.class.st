Class {
	#name : #LGitAbstractError,
	#superclass : #Error,
	#instVars : [
		'object'
	],
	#category : #'LibGit-Core-Exceptions'
}

{ #category : #signalling }
LGitAbstractError class >> signalWith: anObject [
	^ self new
		object: anObject;
		signal
]

{ #category : #initialization }
LGitAbstractError >> initialize [
	super initialize.
	self messageText: LGitError lastMessage
]

{ #category : #accessing }
LGitAbstractError >> object [
	^ object
]

{ #category : #accessing }
LGitAbstractError >> object: anObject [
	object := anObject
]
