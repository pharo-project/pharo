*Versionner-Spec-Browser
whenSelectedNodesChanged: aBlock
	"Set a block to perform when a new node is selected"

	selectionHolder whenChangedDo: [ :col | aBlock cull: col ]