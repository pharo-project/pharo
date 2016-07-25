emulating
snapshotWithElements: anArray
	^ MCSnapshot
		fromDefinitions: (anArray collect: [:t | self mockToken: t])