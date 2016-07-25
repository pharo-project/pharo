brick-structure-events
onRemovedFrom: aBrick
	super onRemovedFrom: aBrick.
	
	"I must not receive any announcements as soon as i'm removed from dom"
	self stepModel ifNotNil: [
		self stepModel announcer unsubscribe: self.
		self stepModel spotter announcer unsubscribe: self  ]