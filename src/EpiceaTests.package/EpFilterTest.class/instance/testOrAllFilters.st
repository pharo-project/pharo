tests
testOrAllFilters
	| aFilter |
	aFilter := EpOrFilter withAll: EpPluggableFilter allFilters.
	
	self diverseEvents do: [ :anEvent |
		self 
			assert: (aFilter accepts: anEvent)
			description: 'The idea is that all events are accepted.' ]