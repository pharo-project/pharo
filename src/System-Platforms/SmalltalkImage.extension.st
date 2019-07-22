Extension { #name : #SmalltalkImage }

{ #category : #'*System-Platforms' }
SmalltalkImage >> os [
	"Answer the object to query about os."
	
	^ OSPlatform current
]

{ #category : #'*System-Platforms' }
SmalltalkImage >> platform [
	"Answer the object to query about os."
	
	^ self os
]

{ #category : #'*System-Platforms' }
SmalltalkImage >> platformName [
	^ self os platformName
]
