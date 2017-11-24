Class {
	#name : #GTSpotterCandidateSelectedMoved,
	#superclass : #Announcement,
	#instVars : [
		'candidateBrick'
	],
	#category : #'GT-Spotter-Announcements'
}

{ #category : #accessing }
GTSpotterCandidateSelectedMoved >> itemBrick [
	^ candidateBrick
]

{ #category : #accessing }
GTSpotterCandidateSelectedMoved >> itemBrick: anObject [
	candidateBrick := anObject
]
