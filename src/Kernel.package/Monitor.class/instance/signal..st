signaling-specific
signal: aSymbolOrNil
	"One process waiting for the given event is woken up. If there is no process waiting 
	for this specific event, a process waiting for the default event gets resumed."

	| queue |
	self checkOwnerProcess.
	queue := self queueFor: aSymbolOrNil.
	queue isEmpty ifTrue: [queue := self defaultQueue].
	self signalQueue: queue.