registration
register: anTool as: aNameSelector
	| usedToolSet |
	anTool ifNil:[ ^ self].
	super register: anTool as: aNameSelector.
	usedToolSet := recentTools at: aNameSelector ifAbsentPut: [ Set new ].
	usedToolSet add: anTool