events
eventLoop
	"Fetch pending raw events from the VM.
	 This method is run at high priority."
	| eventBuffer |

	eventBuffer := Array new: 8.
	
	[true] whileTrue: [
		| type window |
		self waitForInput.

		[self primGetNextEvent: eventBuffer.
		type := eventBuffer at: 1.
		type = EventTypeNone]
			whileFalse: [
				"Patch up the window index in case we don't get one"
				window := eventBuffer at: 8.
				(window isNil
					or: [window isZero])
					ifTrue: [eventBuffer at: 8 put: 1].	
						
				self signalEvent: eventBuffer]]