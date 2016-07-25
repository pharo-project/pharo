registry
register
	self ensureLocalRepository.
	self class registry add: self.
	^ self 
