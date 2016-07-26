menu building
currentRoot: anItem while: aBlock
	| old |
	old := currentRoot.
	currentRoot := anItem.
	[aBlock value] ensure: [currentRoot := old]