Extension { #name : 'MCVersion' }

{ #category : '*MonticelloGUI' }
MCVersion >> browse [

	^ (MCSpecSnapshotBrowser forSnapshot: self completeSnapshot) open
]
