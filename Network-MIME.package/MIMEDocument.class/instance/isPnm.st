testing
isPnm
	^ self mainType = 'image'
		and: [self subType = 'pnm']