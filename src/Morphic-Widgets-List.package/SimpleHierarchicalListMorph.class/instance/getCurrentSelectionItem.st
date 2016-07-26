selection
getCurrentSelectionItem

	^model perform: (getSelectionSelector ifNil: [^nil])
	