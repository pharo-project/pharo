class initialization
initialize
	"in order to update settings at startup.
	shoudl be register at the end of standard classes but before user classes"
	
	"IMPORTANT: do not uncomment the registration to the session manager!
	For now, SystemSettingsPersistence is handled directly by the command line handler startup.
	We should define a startup action parsing command line arguments and able to provide them to other startup actions. Then, SystemSettingsPersistence as well as StartupPreferencesLoader, could be independent startup actions."
	"SessionManager default
		registerBeforeUserClass: self"