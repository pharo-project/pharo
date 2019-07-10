"
I am the object responsible of managing how sessions work in Pharo.
A session defines the boundaries of work in the image.

A new session starts when the image starts.
A session stops when the image quits.
There is only one active session at a single point of time.

The current active session is held by myself, the singleton session manager. It can be accessed by doing:

  SessionManager default currentSession.

The most important responsibility of the session manager is to manage how resources and services in the image are started up and shut down at the beginning and end of a session respectively. For example, when the image starts, several initialization routines should be executed to make sure that the image has access to the graphic drivers, the standard input/output file descriptors and so on.

Such initialization happens in the #snapshot:andQuit: method. #snapshot:andQuit: will:
 - stop current session
 - save current image if requested
 - quit if requested
 - start a new session
 
When a session is started, all elements registered in the startup list are started up.
When a session is stopped, all elements registered in the shutdown list are shut down.

# Managing Startup and Shutdown lists

The startup and shutdown lists can be accessed through the messages:

    SessionManager default startupList.
    SessionManager default shutdownList.

In general terms, the shutdown list is the startup list reversed.

Upon a startup [shutdown], all elements in the startup list are sent the message #startup: [#shutdown:] with a boolean as argument that indicates wether the image is being saved [closed].

Internally, startup and shutdown lists are prioritised. Priorities are managed by startup categories. By default the session manager includes the following categories in decreasing priority order:

- System
- Network
- Graphical User Interface
- Tools
- User

Categories can be accessed as follows:

    SessionManager default categoryNamed: aName.

New categories can be registered in the system using the messages:

    SessionManager default createCategory: aCategoryName.
    SessionManager default createCategory: aCategoryName after: anotherCategoryName.

Finally, to subscribe some resource handler to the startup shutdown lists, we need to subscribe a handler, subclass of AbstractSessionHandler.
The most common handler implementation so far is the ClassSessionHandler, that allows to subscribe a class for startup and shutdown, keeping backwards compatibility to the old startup mechanism.

    ClassSessionHandler forClassNamed: aClassName

We can register a session handler as follows

    SessionManager default
        register: (ClassSessionHandler forClassNamed: self name)
        inCategory: SessionManager default systemCategory.
        
Or alternatively, by talking to the corresponding category:

    SessionManager default systemCategory register: (ClassSessionHandler forClassNamed: self name)

# System Category Priorities

A system category internally prioritizes its elements to provide a fine grained control on the startup and shutdown order.
All methods above have variants that allow developers to specify the priority inside the category:  

    SessionManager default
        register: (ClassSessionHandler forClassNamed: self name)
        inCategory: SessionManager default systemCategory
        atPriority: 100.

    SessionManager default systemCategory
        register: (ClassSessionHandler forClassNamed: self name)
        atPriority: 100
        
By default, if no priority is specified, a default priority is used. Every category answers to the message #defaultPriority.

# How does an image restart from the point it was before

An important point in the image startup is how does it manage to restart from the point where it was executing when it was saved.

When the image is saved, using the snapshot primitive, the entire image is freezed at the point of the snapshot.
More particularly, the process that invoked the snapshot primitive is freezed at the point of the primitive call.
This works as a process fork: the running image will return from the snapshot primitive and the saved file will also start from the freezed point.
To differentiate whether we are executing in the running image or in the freshly-saved image, the snapshot primitive returns a boolean indicating it.

Read the comment of #snapshotPrimitive for more details.
"
Class {
	#name : #SessionManager,
	#superclass : #Object,
	#instVars : [
		'currentSession',
		'categories',
		'guiCategory',
		'toolsCategory',
		'networkCategory',
		'systemCategory',
		'userCategory'
	],
	#classVars : [
		'Default'
	],
	#category : #'System-SessionManager-Base'
}

{ #category : #accessing }
SessionManager class >> default [
	^ Default ifNil: [ Default := self new ] 
]

{ #category : #accessing }
SessionManager class >> default: aSessionManager [
	Default := aSessionManager
]

{ #category : #'compatibility - startupList' }
SessionManager class >> initializedSessionManager [
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
		registerSystemClassNamed: #MemoryStore; "System"
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
		"It Should run at the end of everything to correctly handle the errors"
		registerLast: UIManagerSessionHandler uniqueInstance
			inCategory: manager userCategory; 

		registerToolClassNamed: #CPUWatcher; "Tools"
		registerToolClassNamed: #PharoCommonTools; "Tool"
		registerToolClassNamed: #GTPlayBook; "Tools"

		registerLast: (ClassSessionHandler forClassNamed: #SystemSettingsPersistence)
			inCategory: manager userCategory; "Preferences loading may need to have anything else well initialized before."
		
		yourself.
	^ manager
]

{ #category : #'deferred startup actions' }
SessionManager >> addDeferredStartupAction: aBlock [
	"Add the block to the list of actions that we will be performed immediately after the startup list is executed."
	self currentSession 
		addDeferredStartupAction: aBlock
	
]

{ #category : #'category management' }
SessionManager >> basicCreateCategory: aString [ 
	^ SessionCategory new
		name: aString;
		yourself
]

{ #category : #accessing }
SessionManager >> categoryNamed: aString [ 
	^ categories 
		detect: [ :each | each name = aString ]
		ifNone: [ NotFound signalFor: aString ]
]

{ #category : #'category management' }
SessionManager >> createCategory: aCategoryName [
	| category |
	category := self basicCreateCategory: aCategoryName.
	categories add: category.
	^ category
]

{ #category : #'category management' }
SessionManager >> createCategory: aCategoryName after: anotherCategoryName [
	| category |
	category := self basicCreateCategory: aCategoryName.	
	categories 
		add: category
		after: (self categoryNamed: anotherCategoryName).
	^ category
]

{ #category : #accessing }
SessionManager >> currentSession [
	^ currentSession
]

{ #category : #accessing }
SessionManager >> defaultPriority [
	"Completely arbitrary by now"
	^ 100
]

{ #category : #'well-known categories' }
SessionManager >> guiCategory [
	^ guiCategory
]

{ #category : #registration }
SessionManager >> hasRegistered: handledId [
	"handledId, in most cases, will be a class name"
	
	^ self startupList 
			anySatisfy: [ :handler | handler handledId = handledId ]
	
]

{ #category : #initialization }
SessionManager >> initialize [
	super initialize.
	categories := OrderedCollection new.
		
	systemCategory := self createCategory: 'System'.
	networkCategory := self createCategory: 'Network'.
	guiCategory := self createCategory: 'Graphical User Interface'.
	toolsCategory := self createCategory: 'Tools'.
	userCategory := self createCategory: 'User'.
]

{ #category : #'session management' }
SessionManager >> installNewSession [
	self installSession: self newSession.
]

{ #category : #'session management' }
SessionManager >> installSession: aWorkingSession [ 
	currentSession := aWorkingSession
]

{ #category : #'snapshot and quit' }
SessionManager >> launchSnapshot: save andQuit: quit [
	| isImageStarting snapshotResult |
	ChangesLog default logSnapshot: save andQuit: quit.
	
	self currentSession stop: quit.	"Image not usable from here until the session is restarted!"
	save
		ifTrue: [
					snapshotResult := Smalltalk snapshotPrimitive.	"<-- PC frozen here on image file"
					isImageStarting := snapshotResult == true. ]
		ifFalse: [ isImageStarting := false ].
	(quit and: [ isImageStarting not ])
		ifTrue: [ Smalltalk quitPrimitive ].

	"create a new session object if we're booting"
	isImageStarting ifTrue: [ self installNewSession ].

	self currentSession start: isImageStarting.
	snapshotResult 
		ifNil: [ self error: 'Failed to write image file (disk full?)' ]
		ifNotNil: [ 
			SystemAnnouncer uniqueInstance snapshotDone: isImageStarting ].
	
	"We return the resuming state, which may be useful for users to know the state of the image"
	^ isImageStarting
]

{ #category : #accessing }
SessionManager >> maxPriority [
	"Completely arbitrary by now"
	^ 10000
]

{ #category : #'well-known categories' }
SessionManager >> networkCategory [
	^ networkCategory
]

{ #category : #'session management' }
SessionManager >> newSession [
	| aWorkingSession |
	aWorkingSession := WorkingSession new.
	aWorkingSession manager: self.
	^ aWorkingSession
]

{ #category : #registration }
SessionManager >> register: aSessionHandler [
	self
		register: aSessionHandler
		inCategory: self userCategory
		atPriority: self defaultPriority
]

{ #category : #registration }
SessionManager >> register: aSessionHandler inCategory: aCategory [
	aCategory 
		register: aSessionHandler
		atPriority: aCategory defaultPriority
]

{ #category : #registration }
SessionManager >> register: aSessionHandler inCategory: aCategory atPriority: anInteger [ 
	self unregisterHandler: aSessionHandler handledId.
	aCategory 
		register: aSessionHandler
		atPriority: anInteger
]

{ #category : #registration }
SessionManager >> registerGuiClassNamed: aClassName [
	self 
		register: (ClassSessionHandler forClassNamed: aClassName)
		inCategory: self guiCategory 
		atPriority: self defaultPriority
]

{ #category : #registration }
SessionManager >> registerGuiClassNamed: aClassName atPriority: anInteger [
	self 
		register: (ClassSessionHandler forClassNamed: aClassName)
		inCategory: self guiCategory 
		atPriority: anInteger
]

{ #category : #registration }
SessionManager >> registerLast: aSessionHandler inCategory: aCategory [
	self 
		register: aSessionHandler
		inCategory: aCategory 
		atPriority: self maxPriority
]

{ #category : #registration }
SessionManager >> registerNetworkClassNamed: aClassName [
	self 
		register: (ClassSessionHandler forClassNamed: aClassName)
		inCategory: self networkCategory 
		atPriority: self defaultPriority
]

{ #category : #registration }
SessionManager >> registerSystemClassNamed: aClassName [
	self 
		register: (ClassSessionHandler forClassNamed: aClassName)
		inCategory: self systemCategory 
		atPriority: self defaultPriority
]

{ #category : #registration }
SessionManager >> registerSystemClassNamed: aClassName atPriority: anInteger [
	self 
		register: (ClassSessionHandler forClassNamed: aClassName)
		inCategory: self systemCategory 
		atPriority: anInteger
]

{ #category : #registration }
SessionManager >> registerToolClassNamed: aClassName [
	self 
		register: (ClassSessionHandler forClassNamed: aClassName)
		inCategory: self toolsCategory 
		atPriority: self defaultPriority
]

{ #category : #registration }
SessionManager >> registerUserClassNamed: aClassName [
	self 
		register: (ClassSessionHandler forClassNamed: aClassName)
		inCategory: self userCategory 
		atPriority: self defaultPriority
]

{ #category : #registration }
SessionManager >> registerUserClassNamed: aClassName atPriority: anInteger [
	self 
		register: (ClassSessionHandler forClassNamed: aClassName)
		inCategory: self userCategory 
		atPriority: anInteger
]

{ #category : #registration }
SessionManager >> renamedClass: aClass from: oldName to: newName [
	"Inform SessionManager that aClass has been renamed"
	
	categories do: [ :category |
		category renamedClass: aClass from: oldName to: newName ]
]

{ #category : #accessing }
SessionManager >> shutdownList [
	^ self startupList reversed
]

{ #category : #'snapshot and quit' }
SessionManager >> snapshot: save andQuit: quit [
	| isImageStarting wait |
	"We do the snapshot in a separate process in maximum priority to have always a clean startup.
	This process will be interrupted by the fork, and will be resumed as soon as the snapshot finishes.
	We synchronize these processes in case both are in the same priority.
	When both arguments are false, do nothing and return false."
	(save or: [ quit ])
		ifFalse: [ ^ false ].
	wait := Semaphore new.
	[ isImageStarting := self launchSnapshot: save andQuit: quit.
	  wait signal. ] forkAt: Processor timingPriority - 1.
	wait wait.
	
	"The execution of the deferred startup actions are executed in the caller thread."
	self currentSession executeDeferredStartupActions: isImageStarting.
	
	^ isImageStarting
]

{ #category : #accessing }
SessionManager >> startupList [
	^ (categories flatCollect: #prioritizedList)
		asArray
]

{ #category : #'well-known categories' }
SessionManager >> systemCategory [
	^ systemCategory
]

{ #category : #'well-known categories' }
SessionManager >> toolsCategory [
	^ toolsCategory
]

{ #category : #'session management' }
SessionManager >> uninstallSession: aWorkingSession [ 
	currentSession := nil
]

{ #category : #registration }
SessionManager >> unregisterClassNamed: aClassName [ 
	self unregisterHandler: aClassName
]

{ #category : #registration }
SessionManager >> unregisterHandler: anHandledId [ 
	categories do: [ :each | each unregisterHandler: anHandledId ]
]

{ #category : #'well-known categories' }
SessionManager >> userCategory [
	^ userCategory
]
