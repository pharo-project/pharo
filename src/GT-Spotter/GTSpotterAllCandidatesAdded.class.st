Class {
	#name : #GTSpotterAllCandidatesAdded,
	#superclass : #GTSpotterAnnouncement,
	#instVars : [
		'candidateLinks'
	],
	#category : #'GT-Spotter-Announcements'
}

{ #category : #accessing }
GTSpotterAllCandidatesAdded >> candidateLinks [
	^ candidateLinks
]

{ #category : #accessing }
GTSpotterAllCandidatesAdded >> candidateLinks: anObject [
	candidateLinks := anObject
]
