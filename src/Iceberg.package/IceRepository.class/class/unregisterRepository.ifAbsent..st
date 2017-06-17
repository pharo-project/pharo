registry
unregisterRepository: aRepository ifAbsent: aBlock
	^ self registry 
		remove: aRepository 
		ifAbsent: aBlock 