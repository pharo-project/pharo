drag and drop
acceptDroppingMorph: toDrop event: evt
	complexContents acceptDroppingObject: toDrop complexContents.
	toDrop delete.
	self highlightForDrop: false.