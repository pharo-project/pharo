Extension { #name : #GTSpotterShowPreview }

{ #category : #'*gt-spotter-eventrecorder' }
GTSpotterShowPreview >> asRecorderEvent [

	^ GTSpotterRecorderShowPreviewEvent fromAnnouncement: self
]
