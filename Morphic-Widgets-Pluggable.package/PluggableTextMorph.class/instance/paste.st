menu commands
paste
	self handleEdit: [textMorph editor paste].
	self autoAccept ifTrue: [ self accept ].