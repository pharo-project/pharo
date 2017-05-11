accessing
removeFilter: aFilter

	andFilter := EpAndFilter withAll: (andFilter subFilters copyWithout: aFilter).
	self refresh.