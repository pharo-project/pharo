accessing values
accept: passengerObject droppedOnItem: anItem
	self dropOnItemBlock glamourValue: (GLMMultiValue with: passengerObject with: anItem with: self).
	^ true