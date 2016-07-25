updating
refresh
	"We may be blocking the UI thread, and thus have to draw the world ourselves when necessary"
	lastRefresh := Time millisecondClockValue.
	self currentWorld displayWorld.