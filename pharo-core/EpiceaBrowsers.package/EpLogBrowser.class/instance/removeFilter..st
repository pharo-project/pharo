accessing filters
removeFilter: aFilter

	andFilter := EpAndFilter withAll: (andFilter subFilters copyWithout: aFilter)