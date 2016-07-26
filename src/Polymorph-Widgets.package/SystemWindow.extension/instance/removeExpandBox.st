*Polymorph-Widgets
removeExpandBox
	"Remove the expand box."

	expandBox ifNotNil: [expandBox delete. expandBox := nil]