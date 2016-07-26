actions
loadUpdatesFromFile
	^ self loadUpdatesFromFile: (self optionAt: 'from-file') asFileReference 