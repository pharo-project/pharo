selection
updateIndex
	self index > 0 ifTrue: [self index: (self index min: self maxIndex)]