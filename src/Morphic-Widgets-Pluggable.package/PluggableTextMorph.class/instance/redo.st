menu commands
redo
	self handleEdit: [ textMorph editor redo ].
	self autoAccept ifTrue: [ self accept ].