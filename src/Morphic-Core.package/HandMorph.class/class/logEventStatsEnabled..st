settings
logEventStatsEnabled: aBoolean
	aBoolean
		ifTrue: [ self logEventStatsStart ]
		ifFalse: [ self logEventStatsStop ].