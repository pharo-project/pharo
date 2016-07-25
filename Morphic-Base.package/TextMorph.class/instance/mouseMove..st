event handling
mouseMove: event

	event redButtonPressed ifFalse: [ ^ self ].
	self handleInteraction: [ editor mouseMove: event ]