interactive error protocol
selectFrom: start to: stop
	| result |
	result := super selectFrom: start to: stop.
	self announcer announce: (GLMSelectedTextChanged new start: start; end: stop).
	^ result