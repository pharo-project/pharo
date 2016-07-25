instance creation
created: url
	^ self 
		created: url 
		entity: (ZnEntity textCRLF: 'Created ' , url asString)