spec protocol
encrypted: aBoolean

	self widgetDo: [ :w | 
		w encrypted: aBoolean.
		w changed ]