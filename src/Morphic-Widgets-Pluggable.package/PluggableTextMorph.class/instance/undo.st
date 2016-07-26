menu commands
undo
	self handleEdit: [textMorph editor undo].
	self autoAccept ifTrue: [ self accept ].