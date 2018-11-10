"
I am a commit optimizing history access. I know a history and can determine if I'm merged asking to my history if HEAD history contains me.

All other operations are simply forwarded to my wrapped ice commit.
"
Class {
	#name : #IceHistoryCommit,
	#superclass : #Object,
	#instVars : [
		'iceCommit',
		'history'
	],
	#category : #'Iceberg-Changes'
}

{ #category : #'instance creation' }
IceHistoryCommit class >> commit: anIceCommit history: aHistory [
	
	^ self new
		iceCommit: anIceCommit;
		history: aHistory;
		yourself
]

{ #category : #accessing }
IceHistoryCommit >> ancestors [
	
	^ iceCommit ancestorIds
]

{ #category : #accessing }
IceHistoryCommit >> author [
	
	^ iceCommit author
]

{ #category : #testing }
IceHistoryCommit >> canRevertChanges [
	
	^ iceCommit canRevertChanges
]

{ #category : #accessing }
IceHistoryCommit >> comment [
	
	^ iceCommit comment
]

{ #category : #accessing }
IceHistoryCommit >> datetime [
	
	^ iceCommit datetime
]

{ #category : #'API-changes' }
IceHistoryCommit >> diffFromHead [
	
	^ iceCommit diffFromHead
]

{ #category : #'API-changes' }
IceHistoryCommit >> diffToParent [
	
	^ iceCommit diffToParent
]

{ #category : #accessing }
IceHistoryCommit >> history [
	^ history
]

{ #category : #accessing }
IceHistoryCommit >> history: anObject [
	history := anObject
]

{ #category : #accessing }
IceHistoryCommit >> iceCommit [
	^ iceCommit
]

{ #category : #accessing }
IceHistoryCommit >> iceCommit: anIceGitCommit [ 
	iceCommit := anIceGitCommit
]

{ #category : #accessing }
IceHistoryCommit >> id [
	
	^ iceCommit id
]

{ #category : #testing }
IceHistoryCommit >> isMerged [
	
	^ history headHistory includes: iceCommit
]

{ #category : #accessing }
IceHistoryCommit >> repository [
	
	^ iceCommit repository
]

{ #category : #accessing }
IceHistoryCommit >> shortId [
	
	^ iceCommit shortId
]
