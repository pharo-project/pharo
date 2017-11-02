Extension { #name : #OSPlatform }

{ #category : #'*System-Sources' }
OSPlatform >> potentialLocationsOfSourcesFile [
	"Return a collection of directories where the PharoVXX.sources should be searched."
	^ self subclassResponsibility
]
