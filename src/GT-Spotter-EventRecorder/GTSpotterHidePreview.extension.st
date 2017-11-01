Extension { #name : #GTSpotterHidePreview }

{ #category : #'*gt-spotter-eventrecorder' }
GTSpotterHidePreview >> asRecorderEvent [

	^ GTSpotterRecorderHidePreviewEvent fromAnnouncement: self
]
