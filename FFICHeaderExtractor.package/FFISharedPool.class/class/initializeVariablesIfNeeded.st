variables initialization
initializeVariablesIfNeeded
	"If this shared pool shared pool should be initialized, then let's do it!"
	self shouldInitializeVariables ifTrue: [ 
		self initializeVariables.
	]
	