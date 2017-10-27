compatibility - startupList
initializedSessionManager
	| manager |
	manager := SessionManager new.
	manager
		registerSystemClassNamed: #SmallInteger atPriority: 10; "system - 10"
		registerSystemClassNamed: #Delay atPriority: 20; "System - 20"
		registerSystemClassNamed: #ProcessorScheduler atPriority: 30; "System - 30"
		registerSystemClassNamed: #InputEventFetcher atPriority: 40; "System - 40" "WARNING: should not be in the list if InputEventSensor is already there"
		registerSystemClassNamed: #OSPlatform atPriority: 50; "System - 50"

		registerSystemClassNamed: #Alien atPriority: 60; "System - 60"
		registerSystemClassNamed: #ExternalObject atPriority: 60; "System - 60"
		registerSystemClassNamed: #FFIMethodRegistry atPriority: 60; "System - 60"
		registerSystemClassNamed: #FFICallbackThunk atPriority: 60; "System - 60"
		registerSystemClassNamed: #FileLocator atPriority: 90; "System - 90"
		
		registerSystemClassNamed: #Stdio; "System"
		registerSystemClassNamed: #LanguageEnvironment	; "System"
		registerSystemClassNamed: #ShortIntegerArray; "System"
		registerSystemClassNamed: #DiskStore; "System"
		registerSystemClassNamed: #SmalltalkImage; "System"
		registerSystemClassNamed: #WeakFinalizationList; "System"
		registerSystemClassNamed: #Clipboard; "System"
		registerSystemClassNamed: #MCMethodDefinition; "System"
		registerSystemClassNamed: #Symbol; "System"
		registerSystemClassNamed: #Locale; "System"
		registerSystemClassNamed: #WeakArray; "System"
		registerSystemClassNamed: #FileStream; "System"
		registerSystemClassNamed: #BasicCommandLineHandler; "System"
		registerSystemClassNamed: #NonInteractiveTranscript; "System"
		registerSystemClassNamed: #ASTCache; "System"
		registerSystemClassNamed: #OSEnvironment; "System"
		registerSystemClassNamed: #EndianDetector; "System"

		registerNetworkClassNamed: #InternetConfiguration; "Network"
		registerNetworkClassNamed: #ZnServer; "Network"
		registerNetworkClassNamed: #MCGitHubRepository; "Network"
		registerNetworkClassNamed: #MCGitBasedNetworkRepository; "Network"
		registerNetworkClassNamed: #ZnLogEvent; "Network"
		registerSystemClassNamed: #UUIDGenerator; "Network"

		registerGuiClassNamed: #DisplayScreen atPriority: 10; "GUI - 10"
		registerGuiClassNamed: #InputEventSensor atPriority: 20; "GUI - 20 / Need to create initialize/release methods?"
		
		registerGuiClassNamed: #Cursor; "GUI"	
		registerGuiClassNamed: #Form; "GUI"
		registerGuiClassNamed: #StrikeFont; "GUI"
		registerGuiClassNamed: #Color; "GUI"
		registerGuiClassNamed: #FreeTypeCache; "GUI"
		registerGuiClassNamed: #LogicalFont; "GUI"
		registerGuiClassNamed: #FT2Handle; "GUI"
		registerGuiClassNamed: #FreeTypeSettings; "GUI"
		registerGuiClassNamed: #WorldMorph atPriority: 110; "GUI"
		registerLast: UIManagerSessionHandler uniqueInstance
			inCategory: manager guiCategory; "GUi - last"

		registerToolClassNamed: #CPUWatcher; "Tools"
		registerToolClassNamed: #NOCCompletionTable; "Tool"
		registerToolClassNamed: #Nautilus; "Tool"
		registerToolClassNamed: #PharoCommonTools; "Tool"
		registerToolClassNamed: #GTPlayBook; "Tools"

		registerLast: (ClassSessionHandler forClassNamed: #SystemSettingsPersistence)
			inCategory: manager userCategory; "Preferences loading may need to have anything else well initialized before."
		
		yourself.
	^ manager