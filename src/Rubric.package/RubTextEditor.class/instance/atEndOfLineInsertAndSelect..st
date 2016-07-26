new selection
atEndOfLineInsertAndSelect: aString

	self insertAndSelect: aString at: (self encompassParagraph: self selectionInterval) last + 1.