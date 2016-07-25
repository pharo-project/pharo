top window
updatePanesFromSubmorphs
	"Having removed some submorphs, make sure this is reflected in my paneMorphs."
	paneMorphs := paneMorphs select: [ :pane | submorphs includes: pane ].