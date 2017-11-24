Class {
	#name : #GTSpotterCandidatesAmountChanged,
	#superclass : #GTSpotterAnnouncement,
	#instVars : [
		'amount'
	],
	#category : #'GT-Spotter-Announcements'
}

{ #category : #accessing }
GTSpotterCandidatesAmountChanged >> amount [
	^ amount
]

{ #category : #accessing }
GTSpotterCandidatesAmountChanged >> amount: anObject [
	amount := anObject
]
