selection change
addToSelection: aMorph clickedTheCheckBox: checkBox
	aMorph selected ifTrue: [^false].
	self searchedElement: nil.
	checkBox ifFalse: [ aMorph highlight ].
	aMorph selected: true.
	^ true