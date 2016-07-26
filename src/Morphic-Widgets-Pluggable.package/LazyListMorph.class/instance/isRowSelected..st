row management
isRowSelected: row 
	"Answer true if the arg row is selected"
	^ ((selectedRow notNil
				and: [row = selectedRow])
			or: [listSource itemSelectedAmongMultiple: row])
