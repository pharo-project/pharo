actions
hideActionbar

	(self hasSubbrick: self actionbarBrick) ifTrue: [ 
		self removeBrick: self actionbarBrick ]