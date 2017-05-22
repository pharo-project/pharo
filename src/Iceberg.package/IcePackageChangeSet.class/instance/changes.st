initialization
changes
	^ changes ifNil: [ 
		self analyseChanges. 
		changes ]