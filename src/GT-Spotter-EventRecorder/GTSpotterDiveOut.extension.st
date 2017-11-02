Extension { #name : #GTSpotterDiveOut }

{ #category : #'*gt-spotter-eventrecorder' }
GTSpotterDiveOut >> asRecorderEvent [

	^ GTSpotterRecorderDiveOutEvent fromAnnouncement: self
]
