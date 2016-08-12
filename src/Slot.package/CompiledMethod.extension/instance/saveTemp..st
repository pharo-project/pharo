*Slot
saveTemp: aTemp
	self savedTemps ifNil: [ self propertyAt: #savedTemps put: Set new].
	self savedTemps add: aTemp.