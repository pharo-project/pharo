actions
refresh
	self isOpen ifTrue: [  
			self relationGraph refreshGraph.
			self buildRoots].
		