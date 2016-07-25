protocol
addEntry: aString 
	| d |
	d := DateAndTime now.
	self 
		addEntry: aString
		date: d