cleanup
deleteAllHistory

	self allWatchpoints valuesDo: [ :aWatchpoint| aWatchpoint deleteHistory].