actions
activate

	self isSuspended ifTrue:
		[self timespans add: 
			(Timespan starting: DateAndTime now duration: Duration zero).
		self state: #active]
