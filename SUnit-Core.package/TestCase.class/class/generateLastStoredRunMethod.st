history
generateLastStoredRunMethod

	self shouldGenerateLastStoredRunMethod ifTrue: [
		self class
			compile: (self lastRunMethodNamed: #lastStoredRun)
			classified: 'history' ]