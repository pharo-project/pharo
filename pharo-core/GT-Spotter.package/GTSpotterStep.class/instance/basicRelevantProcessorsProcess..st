private
basicRelevantProcessorsProcess: aSpotterContext
	self basicRelevantProcessorsProcess: aSpotterContext do: [ :processor | 
		processor filterInContext: aSpotterContext ].
	aSpotterContext continueFilter