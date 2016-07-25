model access
getCurrentSelectionIndex
	"Answer the index of the current selection."

	getIndexSelector ifNil: [^0].
	^model perform: getIndexSelector