class initialization
install
	"InputEventFetcher install"
	Default := self new.
	Default startUp.

	SessionManager default
		registerSystemClassNamed: self name 
		atPriority: 40.
