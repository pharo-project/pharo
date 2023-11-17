Extension { #name : 'MCVersion' }

{ #category : '*MonticelloGUI' }
MCVersion >> browse [

	^ (MCSnapshotBrowser forSnapshot: self completeSnapshot)
		showLabelled: 'Snapshot of ', self fileName
]
