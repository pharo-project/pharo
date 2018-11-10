"
I represent an commit that cannot be found in the current repository. I have a commit id/hash, but that commit was not fetched (or was wrongly set). Once the real commit is fetched, I'll be becomed into a real commit.
"
Class {
	#name : #IceUnknownCommit,
	#superclass : #IceCommitish,
	#instVars : [
		'id',
		'datetime'
	],
	#category : #'Iceberg-Core'
}

{ #category : #'API-working copy' }
IceUnknownCommit >> adopt [
	
	"Low level.
	For the working copy to adopt myself as reference commit"
	
	self repository workingCopy adoptCommit: self
]

{ #category : #'API-changes' }
IceUnknownCommit >> changesFromCommit: anIceGitCommit [ 
	^ anIceGitCommit changesFromNoCommit: self
]

{ #category : #'API-changes' }
IceUnknownCommit >> changesTo: aCommitish [

	aCommitish = self
		ifTrue: [ ^ #() ].
	^ aCommitish changesFromNoCommit: self
]

{ #category : #accessing }
IceUnknownCommit >> comment [
	
	^ ''
]

{ #category : #accessing }
IceUnknownCommit >> commit [
	self shouldBeImplemented.
]

{ #category : #accessing }
IceUnknownCommit >> compatibleUsername [
	
	^ 'CompatibleUserName'
]

{ #category : #accessing }
IceUnknownCommit >> datetime [
	
	^ datetime
]

{ #category : #accessing }
IceUnknownCommit >> description [
	^ 'Unknown {1}' format: { self shortId }
]

{ #category : #accessing }
IceUnknownCommit >> fileSystem [
	
	^ FileSystem memory root
]

{ #category : #accessing }
IceUnknownCommit >> id [
	^ id
]

{ #category : #accessing }
IceUnknownCommit >> id: anObject [
	id := anObject
]

{ #category : #initialization }
IceUnknownCommit >> initialize [

	super initialize.
	datetime := DateAndTime now
]

{ #category : #testing }
IceUnknownCommit >> isNoCommit [
	
	^ false
]

{ #category : #testing }
IceUnknownCommit >> isParentOf: aCommit [

	^ false
]

{ #category : #testing }
IceUnknownCommit >> isUnknownCommit [
	
	^ true
]

{ #category : #'API-project' }
IceUnknownCommit >> project [

	^ repository newUnbornProject
]

{ #category : #accessing }
IceUnknownCommit >> shortId [
	
	^ self id first: 7
]
