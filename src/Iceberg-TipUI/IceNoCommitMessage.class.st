"
I indicate when a commit will be performed without a message comment.
This is usually possible, but bad, very bad. Do not do it.
"
Class {
	#name : #IceNoCommitMessage,
	#superclass : #IceWarning,
	#category : #'Iceberg-TipUI-View'
}

{ #category : #accessing }
IceNoCommitMessage class >> defaultMessageText [
	^ 'You are trying to commit without a message!
When this is possible, is not a good practice.'
]

{ #category : #visiting }
IceNoCommitMessage >> acceptError: aVisitor [
	aVisitor visitNoCommitMessage: self
]

{ #category : #accessing }
IceNoCommitMessage >> messageText [
	^ messageText ifNil: [ self class defaultMessageText ]
]
