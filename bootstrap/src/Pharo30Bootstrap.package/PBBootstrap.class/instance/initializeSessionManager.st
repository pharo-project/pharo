bootstrap
initializeSessionManager

	[	'script.st' asFileReference writeStreamDo: [ :str |
		str nextPutAll: '''From Pharo6.0 of 13 May 2016 [Latest update: #60267] on 25 October 2016 at 10:12:07.2002 am''!
	
!WorkingSession methodsFor: ''initialization'' stamp: ''ChristopheDemarey 12/4/2015 11:43''!
initialize
	super initialize.
	deferredStartupActions := OrderedCollection new.
	creationTime := DateAndTime now.
	id := DateAndTime now hash.! !    

		
SessionManager classPool at: #Default put: SessionManager new.
SessionManager default installNewSession.

SessionManager default
		registerSystemClassNamed: #SmallInteger atPriority: 10; "system - 10"
		registerSystemClassNamed: #Delay atPriority: 20; "System - 20"
		registerSystemClassNamed: #ProcessorScheduler atPriority: 30; "System - 30"
		registerSystemClassNamed: #OSPlatform atPriority: 50; "System - 50"
		registerSystemClassNamed: #FileLocator atPriority: 90; "System - 90"
		registerSystemClassNamed: #Stdio; "System"
		registerSystemClassNamed: #LanguageEnvironment	; "System"
		registerSystemClassNamed: #DiskStore; "System"
		registerSystemClassNamed: #SmalltalkImage; "System"
		registerSystemClassNamed: #WeakFinalizationList; "System"
		registerSystemClassNamed: #Symbol; "System"
		registerSystemClassNamed: #Locale; "System"
		registerSystemClassNamed: #MultiByteFileStream; "System"
		registerSystemClassNamed: #WeakArray; "System"
		registerSystemClassNamed: #FileStream; "System"
		registerSystemClassNamed: #BasicCommandLineHandler; "System"
		registerSystemClassNamed: #NonInteractiveTranscript; "System"
		registerSystemClassNamed: #ASTCache; "System"
		registerSystemClassNamed: #EndianDetector. "System"'].
	
		self bootstrapImageEvalFile: 'script.st' save: true quit: true.
	] ensure: [ 'script.st' asFileReference ensureDelete ]