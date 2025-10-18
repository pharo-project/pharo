"
I am announced when full-screen mode state is changed.
"
Class {
	#name : 'FullscreenAnnouncement',
	#superclass : 'SystemAnnouncement',
	#instVars : [
		'fullscreen'
	],
	#category : 'Graphics-Display Objects-Utilities',
	#package : 'Graphics-Display Objects',
	#tag : 'Utilities'
}

{ #category : 'accessing' }
FullscreenAnnouncement >> fullscreen [
	^ fullscreen
]

{ #category : 'accessing' }
FullscreenAnnouncement >> fullscreen: anObject [
	fullscreen := anObject
]
