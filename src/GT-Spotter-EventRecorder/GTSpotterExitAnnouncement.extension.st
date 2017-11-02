Extension { #name : #GTSpotterExitAnnouncement }

{ #category : #'*gt-spotter-eventrecorder' }
GTSpotterExitAnnouncement >> asRecorderEvent [

	^ GTSpotterRecorderExitAnnouncementEvent fromAnnouncement: self
]
