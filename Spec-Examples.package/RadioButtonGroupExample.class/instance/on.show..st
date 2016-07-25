initialization
on: button show: aMorph

	button whenActivatedDo: [ container addMorph: aMorph ].
	button whenDeactivatedDo: [ container removeMorph: aMorph ]