event handling
onRemovedFrom: aBrick
	super onRemovedFrom: aBrick.
	
	self stepModel spotter announcer unsubscribe: self.
	self stepModel announcer unsubscribe: self.
	self stepModel candidates announcer unsubscribe: self