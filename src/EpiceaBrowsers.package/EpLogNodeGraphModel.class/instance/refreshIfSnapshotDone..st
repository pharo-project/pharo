refreshing
refreshIfSnapshotDone: aSnapshotDone

	aSnapshotDone isNewImage ifFalse: [ self refreshLogNodesTreeModel ].
	"Not sure why but it crashes otherwise"
