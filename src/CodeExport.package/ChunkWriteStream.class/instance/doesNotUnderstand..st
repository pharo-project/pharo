reflective operations
doesNotUnderstand: aMessage
	"Forward message to decoree"

	^ aMessage sendTo: decoratedStream