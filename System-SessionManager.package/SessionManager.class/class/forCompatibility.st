compatibility - startupList
forCompatibility
	^ SessionManager new
		registerSystemClassNamed: #SmallInteger; "system - 10"
		registerSystemClassNamed: #Delay; "System - 20"
		registerSystemClassNamed: #Stdio; "System"
		registerSystemClassNamed: #OSPlatform; "System - 50"
		registerSystemClassNamed: #DisplayScreen; "GUI - 10"
		registerSystemClassNamed: #Cursor; "GUI"
		registerSystemClassNamed: #InputEventFetcher; "System - 40" "WARNING: should not be in the list if InputEventSensor is already there"
	
		registerSystemClassNamed: #Form; "GUI"
		registerSystemClassNamed: #StrikeFont; "GUI"
		registerSystemClassNamed: #Color; "GUI"
		
		registerSystemClassNamed: #ProcessorScheduler; "System - 30"
		registerSystemClassNamed: #LanguageEnvironment	; "System"

		registerSystemClassNamed: #NaturalLanguageTranslator; "Does nothing - will not be in the list"

		registerSystemClassNamed: #ShortIntegerArray; "System"
		registerSystemClassNamed: #DiskStore;
		registerSystemClassNamed: #InputEventSensor; "Need to create initialize/release methods?"
		registerSystemClassNamed: #SmalltalkImage; "System"
		registerSystemClassNamed: #WeakFinalizationList; "System"
		registerSystemClassNamed: #CPUWatcher; "Tools"
		registerSystemClassNamed: #Clipboard; "System"
	
		registerSystemClassNamed: #FreeTypeCache; "GUI"
	
		registerSystemClassNamed: #InternetConfiguration; "Network"
	
		registerSystemClassNamed: #LogicalFont; "GUI"
		registerSystemClassNamed: #MCMethodDefinition; "System"
		registerSystemClassNamed: #Symbol; "System"
		
		registerSystemClassNamed: #Locale; "System"
		registerSystemClassNamed: #MultiByteFileStream; "System"
		registerSystemClassNamed: #UUIDGenerator; "Network"
		registerSystemClassNamed: #WeakArray; "System"
		registerSystemClassNamed: #MacOSClipboard; "nothing to do - will not be in  the list"
		registerSystemClassNamed: #ZnServer; "Network"
		registerSystemClassNamed: #FileStream; "System"
		registerSystemClassNamed: #FileLocator; "System"
		registerSystemClassNamed: #BasicCommandLineHandler; "System"
		registerSystemClassNamed: #FT2Handle; "GUI"
		registerSystemClassNamed: #FreeTypeSettings; "GUI"
	
		registerSystemClassNamed: #NonInteractiveTranscript; "System"
		registerSystemClassNamed: #ASTCache; "System"
		registerSystemClassNamed: #NOCCompletionTable; "Tool"
		registerSystemClassNamed: #Nautilus; "Tool"
		registerSystemClassNamed: #PharoCommonTools; "Tool"
		registerSystemClassNamed: #OSEnvironment; "System"
		
		registerSystemClassNamed: #WorldMorph; "GUI"
		registerSystemClassNamed: #MCGitHubRepository; "Network"
		registerSystemClassNamed: #ZnLogEvent; "Network"
		registerSystemClassNamed: #GTPlayBook; "Tools"
		registerSystemClassNamed: #EndianDetector; "System"
		yourself