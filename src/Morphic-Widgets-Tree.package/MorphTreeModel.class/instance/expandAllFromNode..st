expanding-collapsing
expandAllFromNode: aNode
	self  requestView: (MorphTreeChangeRequest expandAllFromNode: aNode)
