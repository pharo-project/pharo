tests
testEquality
	self 
		assert: EpPluggableFilter methodFilter
		equals: EpPluggableFilter methodFilter.
	
	self 
		assert: (EpOrFilter withAll: EpPluggableFilter allFilters)
		equals: (EpOrFilter withAll: EpPluggableFilter allFilters).