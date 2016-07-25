private tree building
parent: aNode while: aBlock
	| oldParent |
	oldParent := currentParent.
	currentParent := aNode.
	aBlock value.
	currentParent := oldParent