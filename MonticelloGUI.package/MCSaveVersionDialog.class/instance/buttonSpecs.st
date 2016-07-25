morphic ui
buttonSpecs
	^ #((Accept accept 'accept version name and log message')
		(Cancel cancel 'cancel saving version')
		('Old log messages...' oldLogMessages 're-use a previous log message')
		) 