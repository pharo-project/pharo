Extension { #name : #GTSpotterSearchFinished }

{ #category : #'*gt-spotter-eventrecorder' }
GTSpotterSearchFinished >> asRecorderEvent [

	^ GTSpotterRecorderSearchFinishedEvent fromAnnouncement: self
]
