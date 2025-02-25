"
I represent a request for user assistance in resolving an origin. I am a resumable exception that gets raised when there is no way of automatically resolving a particular origin. 
"
Class {
	#name : 'ResolutionRequest',
	#superclass : 'Notification',
	#instVars : [
		'origin'
	],
	#category : 'FileSystem-Core-Base-Errors',
	#package : 'FileSystem-Core',
	#tag : 'Base-Errors'
}

{ #category : 'instance creation' }
ResolutionRequest class >> for: origin [
	^ self new
		origin: origin;
		signal
]

{ #category : 'accessing' }
ResolutionRequest >> origin: aSymbol [
	origin := aSymbol
]
