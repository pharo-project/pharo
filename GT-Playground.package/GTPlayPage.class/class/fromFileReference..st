instance creation
fromFileReference: aFileReference
	^ self new
		content: (aFileReference readStreamDo: #contents); 
		creationTime: (self creationTimeFrom: aFileReference);
		yourself