running
debug
	| result |
	result := self classForTestResult new.
	[ result runCaseForDebug: self ]
		ensure: [ self classForTestResource resetResources: self resources.
			result updateResultsInHistory  ].
	^ result