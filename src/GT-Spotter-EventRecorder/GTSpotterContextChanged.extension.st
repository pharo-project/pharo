Extension { #name : #GTSpotterContextChanged }

{ #category : #'*gt-spotter-eventrecorder' }
GTSpotterContextChanged >> asRecorderEvent [

	^ GTSpotterRecorderContextChangedEvent fromAnnouncement: self
]
