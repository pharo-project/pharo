actions
loadUpdates
	[ 
		(self hasOption: 'from-file')
			ifTrue:  [ self loadUpdatesFromFile ]
			ifFalse: [ self loadUpdates ].
	] on: UpdateFileNotLoaded do: [ 
		self shouldForce
			ifFalse: [ self exitFailure ]]