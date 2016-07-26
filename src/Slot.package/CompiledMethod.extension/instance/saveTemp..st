*Slot
saveTemp: aTemp
	self savedTemps ifNil: [ self propertyValueAt: #savedTemps put: Set new].
	self savedTemps add: aTemp.