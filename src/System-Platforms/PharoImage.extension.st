Extension { #name : 'PharoImage' }

{ #category : '*System-Platforms' }
PharoImage >> os [
	"Answer the object to query about os."

	^ OSPlatform current
]

{ #category : '*System-Platforms' }
PharoImage >> platform [
	"Answer the object to query about os."

	^ self os
]

{ #category : '*System-Platforms' }
PharoImage >> platformName [
	^ self os platformName
]
