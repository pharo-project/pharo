instance creation
redirect: url
	^ self
		redirect: url
		entity: (ZnEntity textCRLF: 'Redirect ' , url asString)