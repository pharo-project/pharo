Class {
	#name : #GTSpotterCandidateAdded,
	#superclass : #GTSpotterAnnouncement,
	#instVars : [
		'candidateLink'
	],
	#category : #'GT-Spotter-Announcements'
}

{ #category : #accessing }
GTSpotterCandidateAdded >> candidate [
	^ candidateLink value
]

{ #category : #accessing }
GTSpotterCandidateAdded >> candidateLink [

	^ candidateLink
]

{ #category : #accessing }
GTSpotterCandidateAdded >> candidateLink: anObject [
	candidateLink := anObject
]
