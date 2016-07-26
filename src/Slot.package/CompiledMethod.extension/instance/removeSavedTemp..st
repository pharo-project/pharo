*Slot
removeSavedTemp: aTemp
	self savedTemps ifNotNil: [ :saved | 
			saved remove: aTemp.
			saved ifEmpty: [ self removeProperty: #savedTemps ] ]