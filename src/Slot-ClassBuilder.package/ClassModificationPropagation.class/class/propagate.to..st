instance creation
propagate: aModification to: aClass
	^ self new
		propagate: aModification
		to: aClass