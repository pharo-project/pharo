Extension { #name : #GTSpotterDiveInCategory }

{ #category : #'*gt-spotter-eventrecorder' }
GTSpotterDiveInCategory >> asRecorderEvent [

	^ GTSpotterRecorderDiveInCategoryEvent fromAnnouncement: self
]
