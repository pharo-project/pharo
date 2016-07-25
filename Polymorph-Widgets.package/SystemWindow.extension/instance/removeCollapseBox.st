*Polymorph-Widgets
removeCollapseBox
	"Remove the collapse box."

	collapseBox ifNotNil: [collapseBox delete. collapseBox := nil]