"
A Renraku critic event about an entity
"
Class {
	#name : #ReCritiqueEvent,
	#superclass : #Announcement,
	#instVars : [
		'entity'
	],
	#category : #'Renraku-Announcements'
}

{ #category : #accessing }
ReCritiqueEvent >> entity [
	^ entity
]

{ #category : #accessing }
ReCritiqueEvent >> entity: anObject [

	entity := anObject 
]
