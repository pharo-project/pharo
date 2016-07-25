instance creation
onEnvironment: anEnvironment packages: aCollection
	^ (self onEnvironment: anEnvironment)
		packages: aCollection;
		yourself