private
okHit
	ok := true.
	self directory
		ifNil: [self inform: 'Missing directory']
		ifNotNil: [modalView delete]