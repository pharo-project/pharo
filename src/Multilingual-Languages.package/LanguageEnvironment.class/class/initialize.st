initialization
initialize
	"LanguageEnvironment initialize"

	SessionManager default 
		registerSystemClassNamed: #LanguageEnvironment "prevent subclasses ro register themselves"