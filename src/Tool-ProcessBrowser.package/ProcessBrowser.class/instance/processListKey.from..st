process list
processListKey: aKey from: aView 
	self perform: (self keyEventsDict at: aKey ifAbsent: [ ^ self ])