actions
closeHint

	self asyncTaskUniqueInstance ifNotNil: #terminate.
	self hintBrickUniqueInstance ifNotNil: #close