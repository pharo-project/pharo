instance creation
timeoutSemaphore: aSemaphore afterMSecs: anInteger
	"Create and schedule a Delay to signal the given semaphore when the given number of milliseconds has elapsed. Return the scheduled Delay. The timeout can be cancelled by sending 'unschedule' to this Delay."
	"Details: This mechanism is used to provide a timeout when waiting for an external event, such as arrival of data over a network connection, to signal a semaphore. The timeout ensures that the semaphore will be signalled within a reasonable period of time even if the event fails to occur. Typically, the waiting process cancels the timeout request when awoken, then determines if the awaited event has actually occurred."

	^ (self new setDelay: anInteger forSemaphore: aSemaphore) schedule; yourself
