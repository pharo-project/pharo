submorphs-add/remove
addSubmorphsFromNodeList: aNodeList previouslyExpanded: expandedNodeList
	| morphList  |
	morphList := OrderedCollection new.
	self 
		addMorphsTo: morphList
		from: aNodeList
		withExpandedItems: expandedNodeList
		atLevel: 0.
	self insertNewMorphs: morphList.
	self listManager updateSelectionFromModel.
	self roots do: [:r | r updateChildrenRecursively].
	self updateColumnMorphs
	