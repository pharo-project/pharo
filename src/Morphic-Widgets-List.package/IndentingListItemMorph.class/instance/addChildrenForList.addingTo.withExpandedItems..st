private-container protocol
addChildrenForList: hostList addingTo: morphList withExpandedItems: expandedItems

	firstChild ifNotNil: [
		firstChild withSiblingsDo: [ :aNode | aNode delete].
	].
	firstChild := nil.
	complexContents hasContents ifFalse: [^self].
	firstChild := hostList 
		addMorphsTo: morphList
		from: complexContents contents 
		allowSorting: true
		withExpandedItems: expandedItems
		atLevel: indentLevel + 1.
	