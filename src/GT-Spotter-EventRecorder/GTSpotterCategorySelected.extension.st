Extension { #name : #GTSpotterCategorySelected }

{ #category : #'*gt-spotter-eventrecorder' }
GTSpotterCategorySelected >> asRecorderEvent [

	^ GTSpotterRecorderCategorySelectedEvent fromAnnouncement: self
]
