events-processing
captureEventsUntil: aBlock
"
	Capture all input events, bypassing normal processing flow and redirect all events into block instead.
	Repeat until block will answer true.

	World activeHand captureEventsUntil: [:evt |
		evt isKeyboard and: [ evt keyCharacter = $a ] ]
"

	| release |
	
	release := false.
	
	captureBlock := [:evt | release := aBlock value: evt ].
	
	[
		[ self world doOneCycle. release ] whileFalse.
	] ensure: [
		captureBlock := nil.
	]