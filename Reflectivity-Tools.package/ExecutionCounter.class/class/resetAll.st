cleanup
resetAll
	self allCounters values do: [ :counter | counter reset ].
	Counters := nil