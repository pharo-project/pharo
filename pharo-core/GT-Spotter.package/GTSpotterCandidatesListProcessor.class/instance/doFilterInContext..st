private
doFilterInContext: aSpotterContext
	^ aSpotterContext doFilter: filterBlock gtFilter forProcessor: self