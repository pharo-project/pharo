accessing
defaultWindowModelClass
	| stdout |
	FileStream stdout wantsLineEndConversion: true; converter.
	stdout := VTermOutputDriver stdout.
	stdout nextPutAll: self class name.
	stdout cr.
	^ TickingWindowPresenter