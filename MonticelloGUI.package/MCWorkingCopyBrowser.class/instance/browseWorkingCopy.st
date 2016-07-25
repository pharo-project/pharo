actions
browseWorkingCopy
	workingCopy ifNotNil:
		[(MCSnapshotBrowser forSnapshot: workingCopy completeSnapshot)
			label: 'Snapshot Browser: ', workingCopy packageName;
			show]