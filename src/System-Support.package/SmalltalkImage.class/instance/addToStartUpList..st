startup - shutdown
addToStartUpList: aClass
	"Startup and Shutdown are the responsibility of the SessionManager.
	If you need to register in a specific category, 
	Please use registration methods provided in SessionManager / registration category."

	SessionManager default registerUserClassNamed: aClass name.