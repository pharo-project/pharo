instance creation
notFound: url
	^ self
		notFound: url
		entity: (ZnEntity textCRLF: 'Not Found ' , url asZnUrl pathQueryFragmentPrintString)