"
I keep a diff from a commit to his mail parent.
"
Class {
	#name : #IceTipHistoryDiffToAncestor,
	#superclass : #IceTipHistoryDiffUpdater,
	#category : #'Iceberg-TipUI-View'
}

{ #category : #accessing }
IceTipHistoryDiffToAncestor >> diffFor: aCommitModel [
	^ aCommitModel diffToFirstAncestor
]

{ #category : #accessing }
IceTipHistoryDiffToAncestor >> labelFor: aCommitModel [
	^ '{1} to {2}' format: { 
		aCommitModel shortId.
		aCommitModel ancestorShortIdOrOrigin }
]
