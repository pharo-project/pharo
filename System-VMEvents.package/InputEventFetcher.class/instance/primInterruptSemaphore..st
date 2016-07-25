private events
primInterruptSemaphore: aSemaphore 
	"Primitive. Install the argument as the semaphore to be signalled whenever the user presses the interrupt key. The semaphore will be signaled once each time the interrupt key is pressed."

	<primitive: 134>
	^self primitiveFailed
"Note: This primitive was marked obsolete but is still used and actually quite useful. It could bre replace with a check in the event loop though, without a need for the now obsolete event tickler as event fetching isn't bound to the Morphic loop."