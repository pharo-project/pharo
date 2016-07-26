*Polymorph-Widgets
removeBoxes
	"Remove all label area boxes."

	expandBox ifNotNil: [expandBox delete. expandBox := nil].
	collapseBox ifNotNil: [collapseBox delete. collapseBox := nil]