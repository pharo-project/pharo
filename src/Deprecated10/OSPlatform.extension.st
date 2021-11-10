Extension { #name : #OSPlatform }

{ #category : #'*Deprecated10' }
OSPlatform class >> currentPlatformName [
	"Return the name of the platform we're running on"

	self deprecated: 'Use ''Smalltalk vm operatingSystemName'' instead'.

	^ Smalltalk vm operatingSystemName
]
