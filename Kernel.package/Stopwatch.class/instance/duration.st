accessing
duration

	| ts last |
	self isSuspended 
		ifTrue:
			[ (ts := self timespans) isEmpty ifTrue: 
				[ ts := { Timespan starting: DateAndTime now duration: Duration zero } ] ]
		ifFalse:
			[ last := self timespans last.
			ts := self timespans allButLast
				add: (last duration: (DateAndTime now - last start); yourself);
				yourself ].
		
	^ (ts collect: [ :t | t duration ]) sum
