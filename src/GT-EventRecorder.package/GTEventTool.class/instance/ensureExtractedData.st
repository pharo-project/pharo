api
ensureExtractedData
	self defaultEventsDirectory exists ifFalse: [ self extract ]