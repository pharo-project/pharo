accessing
index: anIndex slotName: aSlotName
	index := anIndex.
	slotName := aSlotName.
	value := thisContext object: self hostObject instVarAt: anIndex