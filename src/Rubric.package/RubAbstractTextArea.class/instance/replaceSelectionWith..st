find-replace
replaceSelectionWith: aText
	self
		handleEdit: [self editor replaceSelectionWith: aText]