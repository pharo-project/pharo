events
signalEvent: eventBuffer
	"Signal the event buffer to all registered event handlers.
	Handlers need make sure to copy the buffer or extract the data otherwise, as the buffer will be reused."

	self eventHandlers do: [:handler |
		handler handleEvent: eventBuffer]