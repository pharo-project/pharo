protocol
toggleSelected
	"Toggle the selection state."

	self takeKeyboardFocus.
	self enabled ifFalse: [^self].
	self model ifNil: [^self].

	(self setStateSelector ifNil: [^self]) numArgs = 0
		ifTrue: [self model perform: self setStateSelector].
	self setStateSelector numArgs = 1
		ifTrue: [	self model perform: self setStateSelector with: self isSelected not].
	self updateSelection.