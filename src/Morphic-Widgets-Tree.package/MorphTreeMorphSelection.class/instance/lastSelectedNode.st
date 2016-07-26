accessing
lastSelectedNode
	^ self lastSelectedNodePath ifNotNil: [:path | path ifEmpty: [nil] ifNotEmpty: [:p | path last]]