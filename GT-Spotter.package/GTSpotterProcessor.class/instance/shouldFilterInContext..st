testing
shouldFilterInContext: aSpotterContext
	^ aSpotterContext notEmpty 
		or: [ self wantsToDisplayOnEmptyQuery ]