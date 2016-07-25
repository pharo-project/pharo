testing
isPng
	^ self mainType = 'image'
		and: [self subType = 'png']