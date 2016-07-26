actions
revert
	| current previous |
	current := self utilityWorkingCopy ancestors first.
	previous := self utilityRepo
		versionFrom: current ancestors first name.
	[ previous load ]
		on: MCMergeOrLoadWarning
		do: [ :warn | warn load ].
	self utilityRepo dictionary removeKey: current.
	currentSnapshot := previous snapshot.