"
I am raised when a morph property changed, and should be propagated to dependents
"
Class {
	#name : #MorphChanged,
	#superclass : #MorphAnnouncement,
	#instVars : [
		'selector'
	],
	#category : #Morphic-Core-Announcements
}

{ #category : #delivering }
MorphChanged >> deliverTo: aHandler [

	^ aHandler update: selector

	
]

{ #category : #accessing }
MorphChanged >> selector [
	^ selector
]

{ #category : #accessing }
MorphChanged >> selector: anObject [
	selector := anObject
]
