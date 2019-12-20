Extension { #name : #UnixPlatform }

{ #category : #'*FileSystem-Core' }
UnixPlatform >> potentialLocationsOfSourcesFile [
	^ { Smalltalk vm directory. FileLocator userData }
]
