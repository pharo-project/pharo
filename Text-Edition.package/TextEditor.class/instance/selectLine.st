new selection
selectLine
	"Make the receiver's selection, if it currently consists of an insertion point only, encompass the current line."
	self hasSelection ifTrue:[^self].
	self selectInterval: (self encompassParagraph: self selectionInterval)