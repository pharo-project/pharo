actions
browseWorkingCopy
	| workingCopy |
	workingCopy := self workingCopy.
	workingCopy
		ifNotNil: [ 
			(MCSnapshotBrowser forSnapshot: workingCopy completeSnapshot)
				label: 'Snapshot Browser: ' , workingCopy packageName;
				show ]