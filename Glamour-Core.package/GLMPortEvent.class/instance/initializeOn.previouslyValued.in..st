initialize-release
initializeOn: aPort previouslyValued: anObject in: aTransmissionContext 
	port := aPort.
	oldValue := anObject.
	transmissionContext := aTransmissionContext