resolving
resolve
	^ self hasResolution
		ifTrue: [MCVersion new
					setPackage: MCSnapshotResource mockPackage
					info: self mockVersionInfo
					snapshot: MCSnapshotResource current snapshot
					dependencies: self children]
		ifFalse: [nil]