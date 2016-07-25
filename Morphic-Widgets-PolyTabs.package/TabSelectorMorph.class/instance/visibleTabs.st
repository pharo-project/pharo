tabs
visibleTabs
	"Answer the tabs that are currently visible."

	^self tabs select: [:t | t owner notNil]