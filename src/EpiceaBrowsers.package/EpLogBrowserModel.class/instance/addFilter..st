menu - operations
addFilter: aFilter

	andFilter := EpAndFilter withAll: (andFilter subFilters copyWith: aFilter).
	self refresh.