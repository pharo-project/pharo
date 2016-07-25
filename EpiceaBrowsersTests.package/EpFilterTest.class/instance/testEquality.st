tests
testEquality
	self 
		assert: EpPluggableFilter behaviorFilter
		equals: EpPluggableFilter behaviorFilter.
	
	self 
		assert: (EpOrFilter withAll: EpPluggableFilter allFilters)
		equals: (EpOrFilter withAll: EpPluggableFilter allFilters).