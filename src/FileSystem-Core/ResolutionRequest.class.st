"
I represent a request for user assistance in resolving an origin. I am a resumable exception that gets raised when there is no way of automatically resolving a particular origin. 
"
Class {
	#name : #ResolutionRequest,
	#superclass : #Notification,
	#instVars : [
		'origin'
	],
	#category : #'FileSystem-Core-Base-Errors'
}

{ #category : #'instance creation' }
ResolutionRequest class >> for: origin [
	^ self new
		origin: origin;
		signal
]

{ #category : #exceptionDescription }
ResolutionRequest >> defaultAction [
	| filedir ref |
	filedir := UIManager default chooseDirectory: 'Where is ', origin asString, '?'.
	ref := filedir ifNotNil: [FileSystem disk referenceTo: filedir fullName].
	self resume: ref
]

{ #category : #accessing }
ResolutionRequest >> origin: aSymbol [
	origin := aSymbol
]
