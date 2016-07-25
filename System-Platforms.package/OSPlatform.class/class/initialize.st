initialize-release
initialize
	"OSPlatform initialize"

	SessionManager default
		registerSystemClassNamed: self name
		atPriority: 50.	
	self startUp: true.