expanding-collapsing
expandedNodesFrom: aMorpList

	^ (aMorpList select: [ :each | each isExpanded]) collect: [ :each | each complexContents].
	