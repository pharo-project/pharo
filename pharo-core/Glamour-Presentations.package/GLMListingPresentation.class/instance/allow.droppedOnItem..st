accessing values
allow: passengerObject droppedOnItem: anItem
	^ self allowDropOnItemBlock glamourValue: (GLMMultiValue with: passengerObject with: anItem with: self)