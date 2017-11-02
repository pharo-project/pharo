"
I am signalled when a repositroy cannot save or access a version.
"
Class {
	#name : #MCPermissionDenied,
	#superclass : #Error,
	#instVars : [
		'repository'
	],
	#category : 'MonticelloRemoteRepositories'
}

{ #category : #signalling }
MCPermissionDenied class >> signalFor: aRepository [
	^ self new
		repository: aRepository;
		signal
]

{ #category : #accessing }
MCPermissionDenied >> repository [
	^ repository
]

{ #category : #accessing }
MCPermissionDenied >> repository: anObject [
	repository := anObject
]
