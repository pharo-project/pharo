Extension { #name : #GTSpotterSelectedCandidateChanged }

{ #category : #'*gt-spotter-eventrecorder' }
GTSpotterSelectedCandidateChanged >> asRecorderEvent [

	^ GTSpotterRecorderSelectedCandidateChangedEvent fromAnnouncement: self
]
