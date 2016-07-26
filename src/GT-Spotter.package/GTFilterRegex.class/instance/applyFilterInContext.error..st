private
applyFilterInContext: aSpotterContext error: exception
	super applyFilterInContext: aSpotterContext error: exception.
	^ (GTFilterSubstrings new
		caseSensitive: self caseSensitive;
		yourself) value: aSpotterContext