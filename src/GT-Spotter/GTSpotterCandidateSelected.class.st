Class {
	#name : #GTSpotterCandidateSelected,
	#superclass : #Announcement,
	#instVars : [
		'candidateBrick'
	],
	#category : #'GT-Spotter-Announcements'
}

{ #category : #accessing }
GTSpotterCandidateSelected >> itemBrick [
	^ candidateBrick
]

{ #category : #accessing }
GTSpotterCandidateSelected >> itemBrick: anObject [
	candidateBrick := anObject
]
