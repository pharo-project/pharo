utils
exitFailure
	[
		self class snapshotErrorImage ifTrue: [
			doNotQuitOnRestart := true.
			"make a new image version snapshot before leaving"
			Smalltalk saveAsNewVersion  ].
	] ensure: [ 
		doNotQuitOnRestart ifFalse: [ Smalltalk exitFailure ].
		doNotQuitOnRestart := false.	
	].
	
	