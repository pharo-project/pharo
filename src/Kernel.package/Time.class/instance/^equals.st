comparing
= aTime

	^ [ self ticks = aTime ticks ]
		on: MessageNotUnderstood do: [false]