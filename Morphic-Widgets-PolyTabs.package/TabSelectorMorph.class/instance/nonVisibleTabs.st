tabs
nonVisibleTabs
	"Answer the tabs that are not currently visible."

	^self tabs select: [:t | t owner isNil]