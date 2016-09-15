refreshing
refresh

	| nodes |
	[	nodes := EpLogNode fromAllLogsIn: self directory.
		nodes := nodes sorted: [ :a :b | a modificationTime >= b modificationTime ] ]
			on: FileSystemError 
			do: [ :error |
				self inform: error asString.
				nodes := #() ].

	logNodesTreeModel roots: nodes.
	