accessing
selectedNodes
	^ self lastSelectedNode
		ifNotNil: [:l | OrderedCollection with: l]
		ifNil: [OrderedCollection new]