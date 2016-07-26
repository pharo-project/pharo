actions
suspend

	| ts |
	self isActive ifTrue:
		[ ts := self timespans last.
		ts duration: (DateAndTime now - ts start).
		self state: #suspended]
