events
outerPortEvent: aPortEvent
	super outerPortEvent: aPortEvent.
	self do: [:each| each outerPortEvent: aPortEvent]