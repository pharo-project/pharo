accessing
selectionIntervalForPane: aSymbol

	^ ((self browser paneNamed: aSymbol) port: #selectionInterval) value ifNil: [ ^ 1 to: 0 ]