events
methodChanged: event
	self default totalNumberOfEntries < 200 ifTrue: [self default quickFillWithCategoryOf: event methodClass].
	self default totalNumberOfEntries < 500 ifTrue: [self default quickFillWithClass: event methodClass].
	self default justCompiled: event selector in: event methodClass.