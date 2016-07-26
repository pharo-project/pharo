private
doContinueFilterForProcessor: aGTProcessor
	self processor: aGTProcessor.
	self filter: ((self cache at: #processorsAndFilters) at: aGTProcessor).
	self streamed: ((self cache at: #processorsAndStreamed) at: aGTProcessor).

	^ aGTProcessor withItemsLimit: aGTProcessor continueItemsLimit do: [ filter value: self ]