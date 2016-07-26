protocol
doubleClick: ann
	ann event shiftPressed ifTrue: [ ^ self openFloatingEditorToRenameAtIndex: ann selectedRowIndex  ].
	self model fullBrowse