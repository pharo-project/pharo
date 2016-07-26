matching
partialMatch

	KMLog logPartialMatch: currentEvent.
	currentEvent ifNotNil:[ currentEvent wasHandled: true ].