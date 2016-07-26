multi-selection
listSelectionAt: index

	index isZero ifFalse:[ lastNonZeroIndex := index ].
	getSelectionListSelector ifNil:[^false].
	^model perform: getSelectionListSelector with: index