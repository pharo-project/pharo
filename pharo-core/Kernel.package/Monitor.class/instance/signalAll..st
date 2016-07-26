signaling-specific
signalAll: aSymbolOrNil
	"All process waiting for the given event or the default event are woken up."

	| queue |
	self checkOwnerProcess.
	queue := self queueFor: aSymbolOrNil.
	self signalAllInQueue: self defaultQueue.
	queue ~~ self defaultQueue ifTrue: [self signalAllInQueue: queue].