accessing
unfocusedSelectionColor
	"Answer the value of selectionColor when the window have not the focus"

	^ unfocusedSelectionColor ifNil: [unfocusedSelectionColor := self selectionColor alphaMixed: 0.7 with: Color veryLightGray lighter]