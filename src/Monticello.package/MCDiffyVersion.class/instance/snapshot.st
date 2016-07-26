accessing
snapshot
	^ snapshot ifNil: [snapshot := MCPatcher apply: patch to: self baseSnapshot]