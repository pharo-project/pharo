event handling
mouseDown: evt
	| now dt |
	self state: #pressed.
	actWhen == #buttonDown
		ifTrue:
			[self doButtonAction]
		ifFalse:
			[now := Time millisecondClockValue.
			super mouseDown: evt.
			"Allow on:send:to: to set the response to events other than actWhen"
			dt := Time millisecondClockValue - now max: 0.  "Time it took to do"
			dt < 200 ifTrue: [(Delay forMilliseconds: 200-dt) wait]].
	self mouseStillDown: evt.