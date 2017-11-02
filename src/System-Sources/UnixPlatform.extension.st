Extension { #name : #UnixPlatform }

{ #category : #'*System-Sources' }
UnixPlatform >> potentialLocationsOfSourcesFile [
	^ { Smalltalk vm directory. FileLocator userData }
]
