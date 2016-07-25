private
giveFocusTo: aMorph

	self hasFocus ifTrue: [aMorph takeKeyboardFocus]