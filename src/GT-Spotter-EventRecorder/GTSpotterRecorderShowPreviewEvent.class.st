"
A GTSpotterRecorderShowPreviewEvent models the action of explicitly closing the preview of an object.

"
Class {
	#name : #GTSpotterRecorderShowPreviewEvent,
	#superclass : #GTSpotterRecorderEvent,
	#category : #'GT-Spotter-EventRecorder-Events'
}

{ #category : #visitor }
GTSpotterRecorderShowPreviewEvent >> accept: aVisitor [
	^ aVisitor visitShowPreviewEvent: self
]

{ #category : #testing }
GTSpotterRecorderShowPreviewEvent >> isShowPreview [
	^ true
]
