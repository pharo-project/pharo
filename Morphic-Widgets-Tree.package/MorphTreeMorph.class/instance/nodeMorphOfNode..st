accessing
nodeMorphOfNode: aNode
	^ self allNodeMorphs detect: [:m | m complexContents  = aNode] ifNone: []