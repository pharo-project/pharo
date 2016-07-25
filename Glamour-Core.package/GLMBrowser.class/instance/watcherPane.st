accessing
watcherPane
	^ watcherPane ifNil: [watcherPane := GLMPane named: #'_watcher']