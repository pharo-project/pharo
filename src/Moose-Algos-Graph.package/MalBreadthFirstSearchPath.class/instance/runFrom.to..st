running
runFrom: startModel to: endModel
	self
		start: (self findNode: startModel);
		end: (self findNode: endModel).
	^ self run