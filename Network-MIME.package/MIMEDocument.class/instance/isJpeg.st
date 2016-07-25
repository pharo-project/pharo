testing
isJpeg
	^ self mainType = 'image'
		and: [self subType = 'jpeg' | (self subType = 'jpg')]