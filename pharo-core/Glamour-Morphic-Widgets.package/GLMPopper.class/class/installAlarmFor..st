utils
installAlarmFor: aBlock
	<deprecated: 'Please use installAlarm: for: instead.
		Installing alarm for a block without existing references is not the best practice as it can be garbage collected and alarm will never be launched.'>
	World
		addAlarm: #value
		withArguments: #()
		for: aBlock
		at: Time millisecondClockValue + 50.