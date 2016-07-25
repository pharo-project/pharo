actions
loadUpdatesFromFile: updatesFile
	updatesFile exists
		ifFalse: [ ^ self exitFailure: (updatesFile fullName, ' does not exist!') ].
	
	UpdateStreamer new updateFromFile: updatesFile.
