new selection
selectLine

	self handleEdit: [ 
		textMorph editor lineSelectAndEmptyCheck: [^ nil] ].