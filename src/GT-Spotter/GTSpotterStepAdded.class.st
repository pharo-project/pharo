Class {
	#name : #GTSpotterStepAdded,
	#superclass : #Announcement,
	#instVars : [
		'stepModel'
	],
	#category : #'GT-Spotter-Announcements'
}

{ #category : #accessing }
GTSpotterStepAdded >> stepModel [
	^ stepModel
]

{ #category : #accessing }
GTSpotterStepAdded >> stepModel: anObject [
	stepModel := anObject
]
