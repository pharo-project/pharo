accessing filters
addFilter: aFilter

	andFilter := EpAndFilter withAll: (andFilter subFilters copyWith: aFilter)