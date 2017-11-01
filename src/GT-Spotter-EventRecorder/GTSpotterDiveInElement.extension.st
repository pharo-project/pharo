Extension { #name : #GTSpotterDiveInElement }

{ #category : #'*gt-spotter-eventrecorder' }
GTSpotterDiveInElement >> asRecorderEvent [

	^ GTSpotterRecorderDiveInElementEvent fromAnnouncement: self
]
