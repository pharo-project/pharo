row management
colorForRow: row 
	"Answer the color for the row text."
	^ (self isRowSelected: row )
		ifTrue: [ self theme selectionTextColor ]
		ifFalse: [ self color ]