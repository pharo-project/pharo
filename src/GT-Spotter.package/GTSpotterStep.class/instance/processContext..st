processing
processContext: aSpotterContext
	self spotter handleExceptionsDuring: [ 
		self stream removeAllCandidates.
		self relevantProcessorsProcess: aSpotterContext  ]