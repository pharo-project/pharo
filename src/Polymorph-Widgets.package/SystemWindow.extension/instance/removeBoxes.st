*Polymorph-Widgets
removeBoxes
	"Remove all label area boxes."

	closeBox ifNotNil: [closeBox delete. closeBox := nil].
	menuBox ifNotNil: [menuBox delete. menuBox := nil].
	expandBox ifNotNil: [expandBox delete. expandBox := nil].
	collapseBox ifNotNil: [collapseBox delete. collapseBox := nil]