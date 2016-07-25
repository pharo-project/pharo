private
primGetNextEvent: array
	"Store the next OS event available into the provided array.
	Essential."
	<primitive: 94>
	 array at: 1 put: EventTypeNone.
     ^nil
	