event handling
requestNextStepWith: origin for: anEventType

	text := ''.
	self pushStep: (self newStep origin: origin; event: anEventType; yourself).
	self steps last process: nil