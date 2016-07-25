initialization
registerWellKnownProcesses
	"Associate each well-known process with a nickname and two flags: allow-stop, and allow-debug.
	Additional processes may be added to this list as required"

	WellKnownProcesses := OrderedCollection new.
	self
		registerWellKnownProcess: [  ]
		label: 'no process'
		allowStop: false
		allowDebug: false.
	self
		registerWellKnownProcess: [ Smalltalk lowSpaceWatcherProcess ]
		label: nil
		allowStop: false
		allowDebug: false.
	self
		registerWellKnownProcess: [ WeakArray runningFinalizationProcess ]
		label: nil
		allowStop: false
		allowDebug: false.
	self
		registerWellKnownProcess: [ Processor backgroundProcess ]
		label: nil
		allowStop: false
		allowDebug: false.
	self
		registerWellKnownProcess: [ InputEventFetcher default fetcherProcess ]
		label: nil
		allowStop: false
		allowDebug: false.
	self
		registerWellKnownProcess: [ UIManager default uiProcess ]
		label: nil
		allowStop: false
		allowDebug: false.
	self
		registerWellKnownProcess: [ Smalltalk globals at: #SoundPlayer ifPresent: [ :sp | sp playerProcess ] ]
		label: 'the Sound Player'
		allowStop: false
		allowDebug: false.
	self
		registerWellKnownProcess: [ Smalltalk globals at: #CPUWatcher ifPresent: [ :cw | cw currentWatcherProcess ] ]
		label: 'the CPUWatcher'
		allowStop: false
		allowDebug: false.
		
	self
		registerWellKnownProcess: [ Delay schedulingProcess ]
		label: nil
		allowStop: false
		allowDebug: false