actions
terminate

	self asyncTask terminate.
	(self isCompleted not & self animatedObject isNotNil) ifTrue: [
		ensureLogic brickValue: self animatedObject ]