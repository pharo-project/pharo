printing
printFrequenceOn: stream
	self frequency > 999
		ifTrue: [ self frequency rounded printWithCommasOn: stream ]
		ifFalse: [ self frequency printOn: stream showingDecimalPlaces: 3 ].
	stream << ' per second'