menu commands
cut
	self handleEdit: [textMorph editor cut].
	self autoAccept ifTrue: [ self accept ].