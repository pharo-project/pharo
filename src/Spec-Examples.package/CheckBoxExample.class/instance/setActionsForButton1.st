initialization
setActionsForButton1

	button1 whenActivatedDo: [ container addMorph: morph1 ].
	button1 whenDeactivatedDo: [ container removeMorph: morph1 ]