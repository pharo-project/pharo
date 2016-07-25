initialization
setActionsForButton2

	button2 whenActivatedDo: [ container addMorph: morph2 ].
	button2 whenDeactivatedDo: [ container removeMorph: morph2 ]