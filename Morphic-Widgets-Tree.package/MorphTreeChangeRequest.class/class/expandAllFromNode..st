instance creation
expandAllFromNode: aNode
	^ self new action: #expandAllFromNode:; arguments: (Array with: aNode)