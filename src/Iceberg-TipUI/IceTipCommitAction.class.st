"
I perform a commit
"
Class {
	#name : #IceTipCommitAction,
	#superclass : #IceTipAction,
	#instVars : [
		'diff',
		'items',
		'message',
		'publish'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #executing }
IceTipCommitAction >> basicExecute [
	self validateCanCommit.
	UIManager default 
		informUser: 'Commiting...' 
		during: [ 
			self repository
				commitChanges: (diff copyWithOnly: items)
				withMessage: message ].
	Iceberg announcer announce: (IceRepositoryModified for: self repository).
	
]

{ #category : #accessing }
IceTipCommitAction >> diff [
	^ diff
]

{ #category : #accessing }
IceTipCommitAction >> diff: anObject [
	diff := anObject
]

{ #category : #accessing }
IceTipCommitAction >> items [
	^ items
]

{ #category : #accessing }
IceTipCommitAction >> items: anObject [
	items := anObject
]

{ #category : #accessing }
IceTipCommitAction >> message [
	^ message
]

{ #category : #accessing }
IceTipCommitAction >> message: anObject [
	message := anObject
]

{ #category : #validating }
IceTipCommitAction >> validateCanCommit [
	self validateMessageNotEmpty
]

{ #category : #validating }
IceTipCommitAction >> validateChangeListNotEmpty [
	"If there are no selected changed, no point on commiting :)"
	
	items ifEmpty: [ IceNothingToCommit signal ]
]

{ #category : #validating }
IceTipCommitAction >> validateMessageNotEmpty [
	self message ifEmpty: [ IceNoCommitMessage signal ]
]
