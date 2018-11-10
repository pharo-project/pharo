"
Representation of a tag
"
Class {
	#name : #IceTag,
	#superclass : #IceCommitish,
	#instVars : [
		'name',
		'commit'
	],
	#category : #'Iceberg-Core'
}

{ #category : #'instance creation' }
IceTag class >> named: aTagName inRepository: anIceRepository [ 
	^ self new name: aTagName; repository: anIceRepository; yourself
]

{ #category : #querying }
IceTag >> commit [
	^ commit ifNil: [ commit := self repository peelTag: self ]
]

{ #category : #'API-tags' }
IceTag >> delete [

	self repository removeTag: self
]

{ #category : #testing }
IceTag >> isTag [

	^ true
]

{ #category : #querying }
IceTag >> lastCommit [
	^ self repository backend revparseCommit: self name
]

{ #category : #accessing }
IceTag >> name [
	^ name
]

{ #category : #accessing }
IceTag >> name: anObject [
	name := anObject
]

{ #category : #'API-history' }
IceTag >> newCommitWalk [

	^ repository newCommitWalk fromTag: self
]

{ #category : #accessing }
IceTag >> repository [
	^ repository
]

{ #category : #accessing }
IceTag >> repository: anObject [
	repository := anObject
]

{ #category : #querying }
IceTag >> versionFor: package [
	^ self commit versionFor: package
]

{ #category : #'as yet unclassified' }
IceTag >> versionsFor: package [ 
	^ self commit versionsFor: package
]
