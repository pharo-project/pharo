*MonticelloGUI
browse

	^ (MCSnapshotBrowser forSnapshot: self completeSnapshot)
		showLabelled: 'Snapshot of ', self fileName