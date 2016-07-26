actions
addRepository
	self class newRepository ifNotNil: [:repos | 
		self addRepository: repos.
		workingCopy ifNil: [ repos morphicOpen: nil ]].
