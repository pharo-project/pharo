selection change
addToSelection: aMorph
	aMorph selected ifTrue: [^false].
	self searchedElement: nil.
	aMorph highlight.
	aMorph selected: true.
	^ true