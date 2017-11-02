Extension { #name : #GTSpotterActOn }

{ #category : #'*gt-spotter-eventrecorder' }
GTSpotterActOn >> asRecorderEvent [

	^ GTSpotterRecorderActOnEvent fromAnnouncement: self
]
