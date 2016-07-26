protocol
justCompiled: selector in: class 
	| date |
	date := DateAndTime now.
	self 
		compiled: selector
		in: class
		date: date