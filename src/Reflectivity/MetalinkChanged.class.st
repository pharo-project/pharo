"
Announce that a metalink changed
"
Class {
	#name : #MetalinkChanged,
	#superclass : #SystemAnnouncement,
	#instVars : [
		'link'
	],
	#category : #Reflectivity-Core
}

{ #category : #accessing }
MetalinkChanged >> link [
	^ link
]

{ #category : #accessing }
MetalinkChanged >> link: anObject [
	link := anObject
]
