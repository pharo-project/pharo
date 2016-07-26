accessing
reporter

	^ reporter ifNil: [ reporter := SlReporter default ]