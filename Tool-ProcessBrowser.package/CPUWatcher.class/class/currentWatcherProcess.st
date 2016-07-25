accessing
currentWatcherProcess
	^CurrentCPUWatcher ifNotNil: [ CurrentCPUWatcher watcherProcess ]
