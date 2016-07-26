class initialization
initialize
	"FileLocator is needed to initialize SystemSettingsPersistence (call to FileLocator>>#preferences),
	and could be useful for other classes.
	Let initialize FileLocator before other standard system classes."
	SessionManager default 
		registerSystemClassNamed: self name atPriority: 90.
	self startUp: true