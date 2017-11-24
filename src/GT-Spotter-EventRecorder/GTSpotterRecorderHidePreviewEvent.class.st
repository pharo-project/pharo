"
A GTSpotterRecorderHidePreviewEvent  models the action of explicitly hidding the preview.

"
Class {
	#name : #GTSpotterRecorderHidePreviewEvent,
	#superclass : #GTSpotterRecorderEvent,
	#category : #'GT-Spotter-EventRecorder-Events'
}

{ #category : #visitor }
GTSpotterRecorderHidePreviewEvent >> accept: aVisitor [
	^ aVisitor visitHidePreviewEvent: self
]

{ #category : #testing }
GTSpotterRecorderHidePreviewEvent >> isHidePreview [
	^ true
]
