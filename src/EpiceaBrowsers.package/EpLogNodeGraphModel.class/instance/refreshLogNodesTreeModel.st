refreshing
refreshLogNodesTreeModel

	| nodes |
	nodes := #().
	[	nodes := EpFileLogNode fromAllLogsIn: self directory.
		nodes := nodes sorted: [ :a :b | a modificationTime >= b modificationTime ] ]
			on: FileSystemError 
			do: [ :error | self inform: error asString ].

	self flag: #fix. "hacky"
	self isMonitorDirectory ifTrue: [
		| currentGlobalName |
		currentGlobalName := self monitor sessionStore store globalName.
		nodes := nodes reject: [:each | each globalName = currentGlobalName ].
		nodes := { EpMonitorLogNode for: self monitor }, nodes.
	].

	hiedraCheckBoxModel state ifTrue: [
		nodes 
			do: [ :node | node populateReferencedNodesWith: nodes ]
			displayingProgress: ('Analyzing ', directory asString)].

	logNodesTreeModel roots: nodes.
