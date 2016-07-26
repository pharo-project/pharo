initialization
currentlyExpanded

	^(scroller submorphs select: [ :each | each isExpanded]) collect: [ :each |
		each complexContents
	].
	