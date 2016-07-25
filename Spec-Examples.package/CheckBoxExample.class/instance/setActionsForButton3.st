initialization
setActionsForButton3

	button3 whenActivatedDo: [ container addMorph: morph3 ].
	button3 whenDeactivatedDo: [ container removeMorph: morph3 ]