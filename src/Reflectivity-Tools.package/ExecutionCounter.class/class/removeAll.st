cleanup
removeAll
	self allCounters values do: [ :aWatchpoint| aWatchpoint uninstall].
	Counters := nil.