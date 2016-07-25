instance creation
unauthorized: authString
	^ self
		unauthorized: authString
		entity: (ZnEntity textCRLF: 'Unauthorized')