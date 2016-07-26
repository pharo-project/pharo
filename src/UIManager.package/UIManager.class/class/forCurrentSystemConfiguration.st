initialization
forCurrentSystemConfiguration
	^ (self allSubclasses
			detect: [ :any | 
				any isValidForCurrentSystemConfiguration
					and: [ any subclasses noneSatisfy: #isValidForCurrentSystemConfiguration ] ]
			ifNone: [ NonInteractiveUIManager ]) new