global events
sendEventWithoutWindow: event
	| converted |
	converted := event accept: self.
	converted ifNil: [ ^ self ].
	globalListeners do: [ :list | list handleEvent: converted ].
	WindowMap valuesDo: [ :window | window deliverGlobalEvent: converted ].