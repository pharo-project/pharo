Extension { #name : #GTSpotterStep }

{ #category : #'*GT-Spotter-UI' }
GTSpotterStep >> diveInCategory [
	"dive in category of selected candidate"
	^ self diveInCategoryDueTo: #unknown
	
]

{ #category : #'*GT-Spotter-UI' }
GTSpotterStep >> diveInCategory: aProcessor dueTo: anEventType [
	"dive in specified aCategory"

	self spotter
		requestNextCategoryStepWith: (self candidates getProcessorLink: aProcessor) for: #diveInCategory;
		notifyDiveInCategory: aProcessor for: anEventType
]

{ #category : #'*GT-Spotter-UI' }
GTSpotterStep >> diveInCategoryDueTo: anEventType [
	"dive in category of selected candidate"

	self diveInCategory: self selected processor dueTo: anEventType
]
