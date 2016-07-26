class initialization
deinstall
	"InputEventFetcher deinstall"

	Default
		ifNotNil: [
			Default shutDown.
			SessionManager default unregisterClassNamed: Default class name.
			Default := nil].

	SessionManager default unregisterClassNamed: self name