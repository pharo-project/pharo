protocol
waitReadyMorphFor: timeoutDuration
	"This method allow wait some small duration until morph will be ready.
	Clients could use it to not show loading progress if it is really fast process.
	Instead some blinking could happen"

	morph ifNotNil: [ ^self].
	retrievingSemaphore wait: timeoutDuration