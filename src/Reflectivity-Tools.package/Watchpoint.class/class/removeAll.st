cleanup
removeAll
	self allWatchpoints values do: [ :aWatchpoint| aWatchpoint uninstall].
	Watchpoints := nil.