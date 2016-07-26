updating
expand
	| c newChildren  |
	isExpanded := true.
	(c := complexContents contents) isEmpty ifTrue: [^self changed].
	newChildren := container addMorphsAfter: self fromCollection: c.
	firstChild := newChildren first.
	self updateChildren

	
