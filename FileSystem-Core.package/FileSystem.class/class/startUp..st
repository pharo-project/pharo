initializing
startUp: aBoolean
	"This is only here to deal with migration from older versions of
	FileSystem that wanted to receive startup notifcations."
	
	SessionManager default unregisterClassNamed: self name