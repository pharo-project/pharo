accessing
selectionInterval: anInterval

	^ ((self browser paneNamed: #code) port: #selectionInterval) value: anInterval