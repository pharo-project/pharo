Extension { #name : #MCVersion }

{ #category : #'*MonticelloGUI' }
MCVersion >> browse [

	^ (MCSnapshotBrowser forSnapshot: self completeSnapshot)
		showLabelled: 'Snapshot of ', self fileName
]

{ #category : #'*MonticelloGUI' }
MCVersion >> open [
	(MCVersionInspector new version: self) show
]
