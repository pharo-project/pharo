current cursor
currentCursor
	"Answer the instance of Cursor that is the one currently displayed."

	^ ActiveHand ifNotNil: [ActiveHand world currentCursor] ifNil: [ CurrentCursor ]