public api
runStartup
	| manager |
	UIManager default inform: 'Running startUp ...'.

	" ---- Following code should be removed once we boostrap a version with slices 18906, 18907 and 18908 integrated 
	(will rely on classes initialization for startup list registration) : "
	SessionManager default: SessionManager initializedSessionManager.
	manager := SessionManager default.
	manager startupList
		select: [ :each | each class = ClassSessionHandler ]
		thenDo: [ :each | 
			[ each registeredClass ]
				on: KeyNotFound
				do: [ manager unregisterClassNamed: each handledId ] ].
	" ---- enf of code to remove "
	
	manager unregisterClassNamed: #BasicCommandLineHandler.	"Do not execute again the script being evaluated"
	manager installNewSession.
	manager currentSession runStartup: true.
	UIManager default inform: 'StartUp done!'