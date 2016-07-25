accessing
findReplaceSelectionColor
	"Answer the value of the selectionColor for find/replace"

	^ findReplaceSelectionColor ifNil: [findReplaceSelectionColor := Color orange alpha: 0.4]