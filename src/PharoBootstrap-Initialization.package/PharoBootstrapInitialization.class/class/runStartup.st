public api
runStartup
	UIManager default inform: 'Running startUp ...'.
	SessionManager default: SessionManager initializedSessionManager.
	SessionManager default installNewSession.
	SessionManager default currentSession runStartup: true.
	UIManager default inform: 'StartUp done!'