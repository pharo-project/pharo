global events
unregisterGlobalListener: globalListener
	"This method registers a global event listener.
	Global event listeners are used to handle events that are not generate for a specific window.
	Joysticks use this kind of events.
	"
	globalListeners remove: globalListener