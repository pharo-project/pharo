event handling
click: anEvent
	self
		handleEdit: [ 
			self editor click: anEvent.
			self scrollSelectionIntoView: nil ]