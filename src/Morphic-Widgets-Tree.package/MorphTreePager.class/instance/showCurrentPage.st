accessing
showCurrentPage
	self buildPanel.
	treeMorph scroller removeAllMorphs.
	(self nodeList isNil or: [self nodeList isEmpty]) 
		ifTrue: [^ treeMorph emptySelection].
	treeMorph addSubmorphsFromNodeList.
	treeMorph updateColumnMorphs.
	treeMorph scrollSelectionIntoView 
