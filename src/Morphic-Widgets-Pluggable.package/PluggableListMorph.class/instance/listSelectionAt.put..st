multi-selection
listSelectionAt: index put: value

	self searchedElement: nil.
	setSelectionListSelector ifNil:[^false].
	^model perform: setSelectionListSelector with: index with: value