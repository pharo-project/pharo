navigation
fullList
	self lastIndex < self nodeList size
		ifTrue: [self nextPage: self nodeList size]