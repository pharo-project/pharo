"
I am announced when full-screen mode state is changed.
"
Class {
	#name : #FullscreenAnnouncement,
	#superclass : #SystemAnnouncement,
	#instVars : [
		'displayScreen',
		'fullscreen'
	],
	#category : #'Graphics-Display Objects-Utilities'
}

{ #category : #accessing }
FullscreenAnnouncement >> displayScreen [
	^ displayScreen
]

{ #category : #accessing }
FullscreenAnnouncement >> displayScreen: anObject [
	displayScreen := anObject
]

{ #category : #accessing }
FullscreenAnnouncement >> fullscreen [
	^ fullscreen
]

{ #category : #accessing }
FullscreenAnnouncement >> fullscreen: anObject [
	fullscreen := anObject
]
