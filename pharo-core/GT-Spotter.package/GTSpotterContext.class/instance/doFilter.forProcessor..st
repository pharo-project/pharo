private
doFilter: aGTFilter forProcessor: aGTProcessor
	self processor: aGTProcessor.
	self filter: aGTFilter.
	self streamed: (OrderedCollection new: 100). " not a limitation, just a reasonable start size "
	(self cache at: #processorsAndFilters) at: aGTProcessor put: aGTFilter.
	(self cache at: #processorsAndStreamed) at: aGTProcessor put: streamed.
	^ aGTFilter value: self