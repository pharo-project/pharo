Class {
	#name : #GLMActionAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'action'
	],
	#category : #Glamour-Morphic-Widgets
}

{ #category : #'instance creation' }
GLMActionAnnouncement class >> action: anAction [
	^self new action: anAction; yourself
]

{ #category : #accessing }
GLMActionAnnouncement >> action [
	^action
]

{ #category : #accessing }
GLMActionAnnouncement >> action: anAction [
	action := anAction
]
