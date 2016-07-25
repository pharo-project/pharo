callbacks
onDoubleClick
	self announcer announce: (GLMTreeMorphStrongSelectionChanged new strongSelectionValue: 
		(self selection ifNotNil: [self selection selectedItemOrItemsOrNil]))