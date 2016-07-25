instance initialization
initializeInstance: anInstance
	self allSlotsDo: [ :slot | slot initialize: anInstance ]