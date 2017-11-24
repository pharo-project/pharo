"
A GTSpotterRecorderDiveInCategoryEvent  models the action of  diving in a category (list of results).

"
Class {
	#name : #GTSpotterRecorderDiveInCategoryEvent,
	#superclass : #GTSpotterRecorderEvent,
	#category : #'GT-Spotter-EventRecorder-Events'
}

{ #category : #visitor }
GTSpotterRecorderDiveInCategoryEvent >> accept: aVisitor [
	^ aVisitor visitDiveInCategoryEvent: self
]

{ #category : #accessing }
GTSpotterRecorderDiveInCategoryEvent >> allFilteredCandidates [
	^ self infoAt: #allFilteredCandidates
]

{ #category : #accessing }
GTSpotterRecorderDiveInCategoryEvent >> categoryTitle [
	^ self infoAt: #categoryTitle
]

{ #category : #accessing }
GTSpotterRecorderDiveInCategoryEvent >> eventType [
	^ self infoAt: #eventType
]

{ #category : #initialization }
GTSpotterRecorderDiveInCategoryEvent >> fromAnnouncement: anAnnouncement [
	super fromAnnouncement: anAnnouncement.
	self infoAt: #eventType put: anAnnouncement eventType.
	self infoAt: #categoryTitle put: anAnnouncement category title.
	self infoAt: #allFilteredCandidates put: anAnnouncement category allFilteredCandidates size.
]

{ #category : #testing }
GTSpotterRecorderDiveInCategoryEvent >> isDiveIn [
	^ true
]
