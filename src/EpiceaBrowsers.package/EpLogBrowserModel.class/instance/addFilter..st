private
addFilter: aFilter

	(andFilter subFilters includes: aFilter) ifTrue: [ ^self ].
	
	andFilter := EpAndFilter withAll: (andFilter subFilters copyWith: aFilter).
	self refresh.