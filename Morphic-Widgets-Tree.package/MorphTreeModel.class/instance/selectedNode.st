accessing
selectedNode
	^ self selectedNodePath ifNotNil: [:path | path ifEmpty: [nil] ifNotEmpty: [path last]]