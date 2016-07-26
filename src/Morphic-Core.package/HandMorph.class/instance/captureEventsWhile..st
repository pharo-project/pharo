events-processing
captureEventsWhile: aBlock
"
	Capture all input events, bypassing normal processing flow and redirect all events into block instead.
	Repeat until block will answer false.

"

	^ self captureEventsUntil: [:evt | (aBlock value: evt) not ]