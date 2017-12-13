"
Change Set:		ProcessBrowser
Date:			14 March 2000
Author:			Ned Konz

email: ned@bike-nomad.com

This is distributed under the Squeak License.

Added 14 March:
	CPUWatcher integration
	automatically start and stop CPUWatcher
	added CPUWatcher to process list menu

Added 29 October:
	MVC version
	2.8, 2.7 compatibility
	rearranged menus
	added pointer inspection and chasing
	added suspend/resume
	recognized more well-known processes
	misc. bug fixes

Added 26 October: highlight pc in source code
Added 27 October: added 'signal semaphore'
added 'inspect receiver', 'explore receiver', 'message tally' to stack list menu
added 'find context', 'next context' to process list menu
added 'change priority' and 'debug' choices to process list menu

27 October mods by Bob Arning:

alters process display in Ned's ProcessBrowser to 
- show process priority
- drop 'a Process in' that appears on each line
- show in priority order
- prettier names for known processes
- fix to Utilities to forget update downloading process when it ends (1 less dead
process)
- correct stack dump for the active process

"
Class {
	#name : #ProcessBrowser,
	#superclass : #Model,
	#instVars : [
		'selectedProcess',
		'selectedContext',
		'methodText',
		'processList',
		'processListIndex',
		'stackList',
		'stackListIndex',
		'selectedClass',
		'selectedSelector',
		'searchString',
		'autoUpdateProcess',
		'deferredMessageRecipient',
		'lastUpdate',
		'startedCPUWatcher',
		'keyEventsDict',
		'textModel'
	],
	#classVars : [
		'SuspendedProcesses',
		'WellKnownProcesses'
	],
	#category : #'Tool-ProcessBrowser'
}

{ #category : #initialization }
ProcessBrowser class >> cleanUp [
 "Remove terminated processes from my suspended list"

       self suspendedProcesses keysDo: [:ea |
               ea isTerminated ifTrue: [self suspendedProcesses removeKey: ea]]
]

{ #category : #'process control' }
ProcessBrowser class >> debugProcess: aProcess [
	self resumeProcess: aProcess.
	aProcess debugWithTitle: 'Interrupted from the Process Browser'.

]

{ #category : #'CPU utilization' }
ProcessBrowser class >> dumpPigStackOn: aStream andClose: aBoolean [
	"Must run forked on its own process, so the monitored behavior is not affected too much" 

	| promise tally process depth stack suspendedContext |
	promise := Processor tallyCPUUsageFor: 1 every: 10.
	tally := promise value.
	"WorldState addDeferredUIMessage: [self dumpTallyOnTranscript: tally]."
	aStream nextPutAll: '====Al processes===='; cr.
	self dumpTally: tally on: aStream.
	aStream cr; nextPutAll: '====Process using most CPU===='; cr.
	process := tally sortedCounts first value.
	(100.0 * (tally occurrencesOf: process) / tally size) rounded printOn: aStream.
	aStream
		nextPutAll: ' % ';
		nextPutAll: (process browserPrintStringWith: (self nameAndRulesFor: process) first);
		cr.
	depth := 20.
	stack := process == Processor activeProcess
		ifTrue: [thisContext stackOfSize: depth]
		ifFalse: [suspendedContext := process suspendedContext.
			suspendedContext
				ifNotNil: [suspendedContext stackOfSize: depth]].
	stack 
		ifNil: [ aStream nextPutAll: 'No context'; cr]
		ifNotNil: [
			stack do: [ :c | 
				c printOn: aStream.
				aStream cr]].
	aBoolean ifTrue: [aStream close]
]

{ #category : #'CPU utilization' }
ProcessBrowser class >> dumpTally: tally on: aStream [
	"tally is from ProcessorScheduler>>tallyCPUUsageFor:
	Dumps lines with percentage of time, hash of process, and a friendly name"

	tally sortedCounts do: [ :assoc | | procName |
		procName := (self nameAndRulesFor: assoc value) first.
		(((assoc key / tally size) * 100.0) roundTo: 1) printOn: aStream.
		aStream
			nextPutAll: '%   ';
			print: assoc value identityHash; space;
			nextPutAll: procName;
			cr.
	]
]

{ #category : #'CPU utilization' }
ProcessBrowser class >> dumpTallyOnTranscript: tally [
	"tally is from ProcessorScheduler>>tallyCPUUsageFor:
	Dumps lines with percentage of time, hash of process, and a friendly name"

	self dumpTally: tally on: Transcript.
	Transcript flush.
]

{ #category : #initialization }
ProcessBrowser class >> initialize [
	"ProcessBrowser initialize"
	SuspendedProcesses ifNil: [ SuspendedProcesses := IdentityDictionary new ].
	self registerWellKnownProcesses
]

{ #category : #'process control' }
ProcessBrowser class >> isUIProcess: aProcess [
	^ aProcess == UIManager default uiProcess
]

{ #category : #'world menu' }
ProcessBrowser class >> menuCommandOn: aBuilder [ 
	<worldMenu> 
	(aBuilder item: #'Process Browser')
		parent: #Tools;
		order: 0.42;
		action:[ self open ];
		help: 'Provides a view of all of the processes (threads) executing in Smalltalk.';
		icon: self taskbarIcon.
]

{ #category : #menu }
ProcessBrowser class >> menuProcessList: aBuilder [ 
	<contextMenu>
	<processBrowserProcessListMenu>

	| target selectedProcess |
	
	target := aBuilder model.
	selectedProcess := target selectedProcess.
	
	selectedProcess
		ifNotNil: [ 
			| nameAndRules |
			nameAndRules := target nameAndRulesForSelectedProcess.
			(aBuilder item: #'Inspect')
				keyText: 'i';
				selector: #inspectProcess.
			(aBuilder item: #'Explore')
				keyText: 'I';
				selector: #exploreProcess.
			(aBuilder item: #'Inspect Pointers')
				keyText: 'P';
				selector: #inspectPointers.
			(Smalltalk globals includesKey: #PointerExplorer)
				ifTrue: [ 
					(aBuilder item: #'Explore pointers')
						keyText: 'e';
						selector: #explorePointers ].
					
			nameAndRules second
				ifTrue: [ 
					(aBuilder item: #'Terminate')
						keyText: 't';
						selector: #terminateProcess.

					selectedProcess isSuspended
						ifTrue: [ 
							(aBuilder item: #'Resume')
								keyText: 'r';
								selector: #resumeProcess ]
						ifFalse: [ 
							(aBuilder item: #'Suspend')
								keyText: 's';
								selector: #suspendProcess ] ].
			
			nameAndRules third
				ifTrue: [ 
					(aBuilder item: #'Change priority')
						keyText: 'p';
						selector: #changePriority.
					(aBuilder item: #'Debug')
						keyText: 'd';
						selector: #debugProcess ].
			
			(aBuilder item: #'Profile messages')
				keyText: 'm';
				selector: #messageTally.

			(selectedProcess suspendingList isKindOf: Semaphore)
				ifTrue: [ 
					(aBuilder item: #'Signal Semaphore')
						keyText: 'S';
						selector: #signalSemaphore ].
					
			(aBuilder item: #'Full stack')
				keyText: 'k';
				selector: #moreStack;
				withSeparatorAfter ].

	(aBuilder item: #'Find context...')
		keyText: 'f';
		selector: #findContext.
	(aBuilder item: #'Find again')
		keyText: 'g';
		selector: #nextContext;
		withSeparatorAfter.
		
	(aBuilder item: (target isAutoUpdating
			ifTrue: [ #'Turn off auto-update' ]
			ifFalse: [ #'Turn on auto-update' ]))
		keyText: 'a';
		selector: #toggleAutoUpdate.
	(aBuilder item: #'Update list')
		keyText: 'u';
		selector: #updateProcessList.

	Smalltalk globals 
		at: #CPUWatcher 
		ifPresent: [ :pw | 
			aBuilder withSeparatorAfter.
			pw isMonitoring
				ifTrue: [ 
					(aBuilder item: #'Stop CPUWatcher')
						selector: #stopCPUWatcher ]
				ifFalse: [ 
					(aBuilder item: #'Start CPUWatcher')
						selector: #startCPUWatcher ] ].
]

{ #category : #menu }
ProcessBrowser class >> menuStackList: aBuilder [
	<contextMenu>
	<processBrowserStackListMenu>
	
	(aBuilder item: #'Inspect context')
		keyText: 'c';
		selector: #inspectContext.
	(aBuilder item: #'Explore context')
		keyText: 'C';
		selector: #exploreContext;
		withSeparatorAfter.
		
	(aBuilder item: #'Inspect receiver')
		keyText: 'i';
		selector: #inspectReceiver.
	(aBuilder item: #'Explore receiver')
		keyText: 'I';
		selector: #exploreReceiver;
		withSeparatorAfter.
		
	(aBuilder item: #'Browse')
		keyText: 'b';
		selector: #browseContext.
]

{ #category : #'process control' }
ProcessBrowser class >> nameAndRulesFor: aProcess [ 
	"Answer a nickname and two flags: allow-stop, and allow-debug"
	| rules |
	rules := {nil. true. true}.
	WellKnownProcesses do: [:blockAndRules |
		blockAndRules key value == aProcess 
			ifTrue: [ rules := blockAndRules value value ]].
	rules first ifNil: [ 
		rules at: 1 put: aProcess suspendedContext asString ].
	^ rules
]

{ #category : #'instance creation' }
ProcessBrowser class >> open [
	"ProcessBrowser open"
	"Create and schedule a ProcessBrowser."
	Smalltalk garbageCollect.
	^ self new build
]

{ #category : #'tools registry' }
ProcessBrowser class >> registerToolsOn: registry [
	"Add ourselves to registry. See [Smalltalk tools]" 
	registry register: self as: #processBrowser

]

{ #category : #'process control' }
ProcessBrowser class >> registerWellKnownProcess: aBlockForProcess label: nickname allowStop: allowStop allowDebug: allowDebug [
	"Add an entry to the registry of well known processes. aBlockForProcess
	evaluates to a known process to be identified by nickname, and allowStop
	and allowDebug are flags controlling allowable actions for this process
	in the browser."

	WellKnownProcesses add: aBlockForProcess->[{nickname . allowStop . allowDebug}]
]

{ #category : #initialization }
ProcessBrowser class >> registerWellKnownProcesses [
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
]

{ #category : #'process control' }
ProcessBrowser class >> resumeProcess: aProcess [
	| priority |
	priority := self suspendedProcesses
				removeKey: aProcess
				ifAbsent: [aProcess priority].
	aProcess priority: priority.
	aProcess suspendedContext ifNotNil: [ aProcess resume ]

]

{ #category : #'process control' }
ProcessBrowser class >> setProcess: aProcess toPriority: priority [
	| oldPriority |
	oldPriority := self suspendedProcesses at: aProcess ifAbsent: [ ].
	oldPriority ifNotNil: [ self suspendedProcesses at: aProcess put: priority ].
	aProcess priority: priority.
	^oldPriority
]

{ #category : #'process control' }
ProcessBrowser class >> suspendProcess: aProcess [
	| priority |
	priority := aProcess priority.
	self suspendedProcesses at: aProcess put: priority.
	"Need to take the priority down below the caller's
	so that it can keep control after signaling the Semaphore"
	(aProcess suspendingList isKindOf: Semaphore)
		ifTrue: [aProcess priority: Processor lowestPriority.
			aProcess suspendingList signal].
	[aProcess suspend]
		on: Error
		do: [:ex | self suspendedProcesses removeKey: aProcess].
	aProcess priority: priority.

]

{ #category : #'process control' }
ProcessBrowser class >> suspendedProcesses [
	"Answer a collection of processes that my instances have suspended.  
	This is so that they don't get garbage collected."
	^ SuspendedProcesses
		ifNil: [SuspendedProcesses := IdentityDictionary new]
]

{ #category : #'CPU utilization' }
ProcessBrowser class >> tallyCPUUsageFor: seconds [
	"Compute CPU usage using a 10-msec sample for the given number of seconds,
	then dump the usage statistics on the Transcript. The UI is free to continue, meanwhile"
	"ProcessBrowser tallyCPUUsageFor: 10"
	^self tallyCPUUsageFor: seconds every: 10
]

{ #category : #'CPU utilization' }
ProcessBrowser class >> tallyCPUUsageFor: seconds every: msec [
	"Compute CPU usage using a msec millisecond sample for the given number of seconds,
	then dump the usage statistics on the Transcript. The UI is free to continue, meanwhile"
	"ProcessBrowser tallyCPUUsageFor: 10 every: 100"

	| promise |
	promise := Processor tallyCPUUsageFor: seconds every: msec.

	[ | tally |
		tally := promise value.
		UIManager default defer: [ self dumpTallyOnTranscript: tally ].
	] fork.
]

{ #category : #icons }
ProcessBrowser class >> taskbarIconName [
	"Answer the icon for the receiver in a task bar."

	^#processBrowserIcon
]

{ #category : #'process control' }
ProcessBrowser class >> terminateProcess: aProcess [
	aProcess ifNotNil: [
		self suspendedProcesses
			removeKey: aProcess
			ifAbsent: [].
		aProcess terminate
	].

]

{ #category : #theme }
ProcessBrowser class >> theme [
	^ Smalltalk ui theme
]

{ #category : #'process control' }
ProcessBrowser class >> unregisterWellKnownProcess: aProcess [
	"Remove the first registry entry that matches aProcess. Use
	with caution if more than one registry entry may match aProcess."

	"self unregisterWellKnownProcess: Smalltalk lowSpaceWatcherProcess"

	WellKnownProcesses detect: [ :e | e key value == aProcess ] ifFound: [ :entry | WellKnownProcesses remove: entry ]
]

{ #category : #'process control' }
ProcessBrowser class >> wasProcessSuspendedByProcessBrowser: aProcess [
	^self suspendedProcesses includesKey: aProcess
]

{ #category : #'stack list' }
ProcessBrowser >> browseContext [
	selectedContext ifNil: [ ^ self ]. 
	Smalltalk tools browser openOnClass: self selectedClass selector: self selectedSelector
]

{ #category : #views }
ProcessBrowser >> browsedEnvironment [

	^ Smalltalk globals
]

{ #category : #views }
ProcessBrowser >> build [
	| window  scrolledText |
	window := (SystemWindow labelled: 'later') model: self.
	self deferredMessageRecipient: WorldState.
	window
		addMorph: ((PluggableListMorph
				on: self
				list: #processNameList
				selected: #processListIndex
				changeSelected: #processListIndex:
				menu: #processListMenu:
				keystroke: #processListKey:from:)
				enableDragNDrop: false)
		frame: (0 @ 0 extent: 0.5 @ 0.5).
	window
		addMorph: ((PluggableListMorph
				on: self
				list: #stackNameList
				selected: #stackListIndex
				changeSelected: #stackListIndex:
				menu: #stackListMenu:
				keystroke: #stackListKey:from:)
				enableDragNDrop: false)
		frame: (0.5 @ 0.0 extent: 0.5 @ 0.5).
		
	textModel setText: self methodText.
	textModel clearUserEdits.
	scrolledText := textModel newScrolledText beForSmalltalkCode; yourself.
	window
		addMorph:  scrolledText
		frame: (0 @ 0.5 corner: 1 @ 1).
	window setUpdatablePanesFrom: #(#processNameList #stackNameList ).
	(window setLabel: 'Process Browser') openInWorld.
	self mayBeStartCPUWatcher.
	^ window
]

{ #category : #'process actions' }
ProcessBrowser >> changePriority [
	| str newPriority nameAndRules |
	nameAndRules := self nameAndRulesForSelectedProcess.
	nameAndRules third
		ifFalse: [self inform: 'Nope, won''t change priority of ' , nameAndRules first.
			^ self].
	str := UIManager default 
				request: 'New priority' 
		  initialAnswer: selectedProcess priority asString.
	str ifNil: [str := String new].
	newPriority := str asNumber asInteger.
	newPriority
		ifNil: [^ self].
	(newPriority < 1
			or: [newPriority > Processor highestPriority])
		ifTrue: [self inform: 'Bad priority'.
			^ self].
	self class setProcess: selectedProcess toPriority: newPriority.
	self updateProcessList
]

{ #category : #'stack list' }
ProcessBrowser >> changeStackListTo: aCollection [ 

        stackList := aCollection.
        self changed: #stackNameList.
        self stackListIndex: 0.
	textModel setText: self methodText. 
]

{ #category : #'process actions' }
ProcessBrowser >> debugProcess [
	| nameAndRules |
	nameAndRules := self nameAndRulesForSelectedProcess.
	nameAndRules third
		ifFalse: [self inform: 'Nope, won''t debug ' , nameAndRules first.
			^ self].
	self class debugProcess: selectedProcess.
]

{ #category : #accessing }
ProcessBrowser >> deferredMessageRecipient: anObject [
	deferredMessageRecipient := anObject
]

{ #category : #accessing }
ProcessBrowser >> doItContext [
	^selectedContext
]

{ #category : #accessing }
ProcessBrowser >> doItReceiver [
	^selectedContext ifNil: [ selectedProcess ] ifNotNil: [ selectedContext receiver ]
]

{ #category : #'stack list' }
ProcessBrowser >> exploreContext [
	selectedContext inspect
]

{ #category : #'process actions' }
ProcessBrowser >> explorePointers [
	| saved |
	selectedProcess ifNil: [ ^ self ].
	saved := selectedProcess.
	[ selectedProcess := nil.
	(Smalltalk tools hasToolNamed: #pointerExplorer)
		ifTrue: [ Smalltalk tools pointerExplorer openOn: saved ]
		ifFalse: [ self inspectPointers ] ]
		     ensure: [ selectedProcess := saved ]
]

{ #category : #'process list' }
ProcessBrowser >> exploreProcess [
	selectedProcess inspect
]

{ #category : #'stack list' }
ProcessBrowser >> exploreReceiver [
	selectedContext ifNotNil: [ selectedContext receiver inspect ]
]

{ #category : #'process list' }
ProcessBrowser >> findContext [
	| initialProcessIndex initialStackIndex found |
	initialProcessIndex := self processListIndex.
	initialStackIndex := self stackListIndex.
	searchString := UIManager default 
			request: 'Enter a string to search for in the process stack lists'
	  initialAnswer: searchString.
	searchString isEmptyOrNil
		ifTrue: [^ false].
	self processListIndex: 1.
	self stackListIndex: 1.
	found := self nextContext.
	found
		ifFalse: [self processListIndex: initialProcessIndex.
			self stackListIndex: initialStackIndex].
	^ found
]

{ #category : #views }
ProcessBrowser >> hasView [
	^self dependents isEmptyOrNil not
]

{ #category : #initialization }
ProcessBrowser >> initialize [
	super initialize.
	self class registerWellKnownProcesses.
	methodText := ''.
	stackListIndex := 0.
	searchString := ''.
	lastUpdate := 0.
	textModel :=  RubScrolledTextModel new interactionModel: self; yourself.
	startedCPUWatcher := CPUWatcher cpuWatcherEnabled and: [ self startCPUWatcher ].
	self updateProcessList; processListIndex: 1
]

{ #category : #'stack list' }
ProcessBrowser >> inspectContext [
	selectedContext inspect
]

{ #category : #'process actions' }
ProcessBrowser >> inspectPointers [
	| tc pointers |
	selectedProcess ifNil: [ ^ self ].
	tc := thisContext.
	pointers := selectedProcess pointersToExcept: { self processList. tc. self}.
	pointers ifEmpty: [ ^ self ].
	pointers inspectWithLabel: 'Objects pointing to ' , selectedProcess browserPrintString
]

{ #category : #'process list' }
ProcessBrowser >> inspectProcess [
	selectedProcess inspect
]

{ #category : #'stack list' }
ProcessBrowser >> inspectReceiver [
	selectedContext
		ifNotNil: [selectedContext receiver inspect]
]

{ #category : #updating }
ProcessBrowser >> isAutoUpdating [
	^autoUpdateProcess notNil and: [ autoUpdateProcess isSuspended  not ]
]

{ #category : #updating }
ProcessBrowser >> isAutoUpdatingPaused [
	^autoUpdateProcess notNil and: [ autoUpdateProcess isSuspended ]
]

{ #category : #'process list' }
ProcessBrowser >> keyEventsDict [

	^ keyEventsDict ifNil: [
		keyEventsDict := Dictionary newFromPairs:  #(
			$i #inspectProcess
			$I #exploreProcess
			$e #explorePointers
			$P #inspectPointers
			$t #terminateProcess
			$r #resumeProcess
			$s #suspendProcess
			$d #debugProcess
			$p #changePriority
			$m #messageTally
			$f #findContext
			$g #nextContext
			$a #toggleAutoUpdate
			$u #updateProcessList
			$S #signalSemaphore
			$k #moreStack			
			)
		]

]

{ #category : #initialization }
ProcessBrowser >> mayBeStartCPUWatcher [
	startedCPUWatcher ifTrue: [ self setUpdateCallbackAfter: 7 ].

]

{ #category : #'stack list' }
ProcessBrowser >> messageTally [
	| secString secs |
	secString := UIManager default request: 'Profile for how many seconds?' initialAnswer: '4'.
	secString ifNil: [secString := String new].
	secs := secString asNumber asInteger.
	(secs isNil
			or: [secs isZero])
		ifTrue: [^ self].
	[ Smalltalk tools timeProfiler spyOnProcess: selectedProcess forMilliseconds: secs * 1000 ] forkAt: selectedProcess priority + 1.
]

{ #category : #accessing }
ProcessBrowser >> methodText [
	^ methodText ifNil: [methodText := selectedContext
						ifNil: ['']
						ifNotNil: [| pcRange | 
							methodText := [ selectedContext sourceCode ]
								ifError: [ 'error getting method text' ].
							pcRange := self pcRange.
							methodText asText
								addAttribute: TextColor red
								from: pcRange first
								to: pcRange last;
								addAttribute: TextEmphasis bold
								from: pcRange first
								to: pcRange last]]
]

{ #category : #'stack list' }
ProcessBrowser >> moreStack [
	self updateStackList: 2000
]

{ #category : #'process actions' }
ProcessBrowser >> nameAndRulesFor: aProcess [ 
	"Answer a nickname and two flags: allow-stop, and allow-debug"
	aProcess == autoUpdateProcess ifTrue: [ ^{'my auto-update process'. true. true} ].
	^self class nameAndRulesFor: aProcess 

]

{ #category : #'process actions' }
ProcessBrowser >> nameAndRulesForSelectedProcess [
	"Answer a nickname and two flags: allow-stop, and allow-debug"
	^self nameAndRulesFor: selectedProcess
]

{ #category : #'process list' }
ProcessBrowser >> nextContext [
	| initialProcessIndex initialStackIndex found |
	searchString isEmpty ifTrue: [ ^false ].
	initialProcessIndex := self processListIndex.
	initialStackIndex := self stackListIndex.
	found := false.
	initialProcessIndex
		to: self processList size
		do: [:pi | found
				ifFalse: [self processListIndex: pi.
					self stackNameList
						withIndexDo: [:name :si | (found not
									and: [pi ~= initialProcessIndex
											or: [si > initialStackIndex]])
								ifTrue: [(name includesSubstring: searchString)
										ifTrue: [self stackListIndex: si.
											found := true]]]]].
	found
		ifFalse: [self processListIndex: initialProcessIndex.
			self stackListIndex: initialStackIndex].
	^ found
]

{ #category : #'process list' }
ProcessBrowser >> notify: errorString at: location in: aStream [ 
	"A syntax error happened when I was trying to highlight my pc. 
	Raise a signal so that it can be ignored."
	Warning signal: 'syntax error'
]

{ #category : #updating }
ProcessBrowser >> pauseAutoUpdate [
	self isAutoUpdating
		ifTrue: [ autoUpdateProcess suspend ].
	self updateProcessList
]

{ #category : #'stack list' }
ProcessBrowser >> pcRange [
	"Answer the indices in the source code for the method corresponding to  
	the selected context's program counter value."
	(selectedContext isNil or: [methodText isEmptyOrNil])
		ifTrue: [^ 1 to: 0].
	^selectedContext debuggerMap
		rangeForPC: (selectedContext pc ifNotNil: [:pc| pc] ifNil: [selectedContext method endPC])
		contextIsActiveContext: stackListIndex = 1
]

{ #category : #'process list' }
ProcessBrowser >> prettyNameForProcess: aProcess [ 
	| nameAndRules |
	aProcess ifNil: [ ^'<nil>' ].
	nameAndRules := self nameAndRulesFor: aProcess.
	^ aProcess browserPrintStringWith: nameAndRules first
]

{ #category : #accessing }
ProcessBrowser >> processList [
	^ processList
]

{ #category : #accessing }
ProcessBrowser >> processListIndex [
	^ processListIndex
]

{ #category : #accessing }
ProcessBrowser >> processListIndex: index [ 
	processListIndex := index.
	selectedProcess := processList
				at: index
				ifAbsent: [].
	self updateStackList.
	self changed: #processListIndex.
]

{ #category : #'process list' }
ProcessBrowser >> processListKey: aKey from: aView [ 
	self perform: (self keyEventsDict at: aKey ifAbsent: [ ^ self ])
]

{ #category : #'process list' }
ProcessBrowser >> processListMenu: menu [
	^menu addAllFromPragma: 'processBrowserProcessListMenu' target: self
]

{ #category : #'process list' }
ProcessBrowser >> processNameList [
	"since processList is a WeakArray, we have to strengthen the result"
	| tally |
	tally := CPUWatcher ifNotNil: [ CPUWatcher current ifNotNil: [ CPUWatcher current tally ] ].
	^ (processList asOrderedCollection copyWithout: nil)
		collect: [ :each | 
			| percent |
			percent := tally
				ifNotNil: [ (((tally occurrencesOf: each) * 100.0 / tally size roundTo: 1) asString padLeftTo: 2) , '% ' ]
				ifNil: [ '' ].
			percent , (self prettyNameForProcess: each) ]
]

{ #category : #views }
ProcessBrowser >> refactor [

	^ NautilusRefactoring model: self
]

{ #category : #'process actions' }
ProcessBrowser >> resumeProcess [
	selectedProcess
		ifNil: [^ self].
	self class resumeProcess: selectedProcess.
	self updateProcessList
]

{ #category : #accessing }
ProcessBrowser >> selectedClass [
	"Answer the class in which the currently selected context's method was  found."
	^ selectedClass
		ifNil: [ selectedClass := selectedContext receiver
				ifNil: [ selectedSelector := selectedContext method selector.
					   selectedContext method methodClass ]
				ifNotNil: [ selectedContext methodClass ]]
]

{ #category : #accessing }
ProcessBrowser >> selectedClassOrMetaClass [
	^self doItReceiver class
]

{ #category : #accessing }
ProcessBrowser >> selectedMethod [
	^selectedContext ifNotNil: [ selectedContext method ]
]

{ #category : #accessing }
ProcessBrowser >> selectedProcess [
	^selectedProcess
]

{ #category : #accessing }
ProcessBrowser >> selectedSelector [
	"Answer the class in which the currently selected context's method was  
	found."
	^ selectedSelector
		ifNil: [selectedSelector := selectedContext receiver
				ifNil: [selectedClass := selectedContext method methodClass
					   selectedContext method selector]
				ifNotNil: [selectedContext selector]]
]

{ #category : #updating }
ProcessBrowser >> setUpdateCallbackAfter: seconds [
	deferredMessageRecipient
		ifNotNil: [ 
			| d |
			d := Delay forSeconds: seconds.
			[ d wait.
			d := nil. 
			deferredMessageRecipient defer: [ self updateProcessList ] ] fork ]
]

{ #category : #'process actions' }
ProcessBrowser >> signalSemaphore [
	(selectedProcess suspendingList isKindOf: Semaphore)
		ifFalse: [^ self].
	[selectedProcess suspendingList signal] fork.
	(Delay forMilliseconds: 300) wait.
	"Hate to make the UI wait, but it's convenient..."
	self updateProcessList
]

{ #category : #accessing }
ProcessBrowser >> stackList [
	^ stackList
]

{ #category : #accessing }
ProcessBrowser >> stackListIndex [
	^ stackListIndex
]

{ #category : #accessing }
ProcessBrowser >> stackListIndex: index [ 
	stackListIndex := index.
	selectedContext := (stackList notNil
						and: [index > 0]) ifTrue:
							[stackList at: index ifAbsent: []].
	selectedClass := nil.
	selectedSelector := nil.
	methodText := nil.
	textModel setText: self methodText
]

{ #category : #views }
ProcessBrowser >> stackListKey: aKey from: aView [ 
	aKey = $c ifTrue: [ self inspectContext].
	aKey = $C ifTrue: [ self exploreContext].
	aKey = $i ifTrue: [ self inspectReceiver].
	aKey = $I ifTrue: [ self exploreReceiver].
	aKey = $b ifTrue: [ self browseContext].


]

{ #category : #'stack list' }
ProcessBrowser >> stackListMenu: aMenu [ 
	selectedContext ifNil: [^ aMenu].
	^aMenu addAllFromPragma: 'processBrowserStackListMenu' target: self
]

{ #category : #'stack list' }
ProcessBrowser >> stackNameList [
	^ stackList
		ifNil: [#()]
		ifNotNil: [stackList collect: [:each | each asString]]
]

{ #category : #updating }
ProcessBrowser >> startAutoUpdate [
	self isAutoUpdatingPaused ifTrue: [ ^autoUpdateProcess resume ].
	self isAutoUpdating
		ifFalse: [| delay | 
			delay := Delay forSeconds: 2.
			autoUpdateProcess := [[self hasView]
						whileTrue: [delay wait.
							deferredMessageRecipient ifNotNil: [
								deferredMessageRecipient defer: [self updateProcessList]]
							ifNil: [ self updateProcessList ]].
					autoUpdateProcess := nil] fork].
	self updateProcessList
]

{ #category : #initialization }
ProcessBrowser >> startCPUWatcher [
	"Answers whether I started the CPUWatcher"

	CPUWatcher isMonitoring
		ifFalse: [ 
					CPUWatcher startMonitoringPeriod: 5 rate: 100 threshold: 0.85.
					self setUpdateCallbackAfter: 7.
					^ true ]
]

{ #category : #updating }
ProcessBrowser >> stopAutoUpdate [
	autoUpdateProcess ifNotNil: [
		autoUpdateProcess terminate.
		autoUpdateProcess := nil].
	self updateProcessList
]

{ #category : #initialization }
ProcessBrowser >> stopCPUWatcher [

	CPUWatcher stopMonitoring.
	self updateProcessList.
	startedCPUWatcher := false	"so a manual restart won't be killed later" 
]

{ #category : #'process actions' }
ProcessBrowser >> suspendProcess [
	| nameAndRules |
	selectedProcess isSuspended
		ifTrue: [^ self].
	nameAndRules := self nameAndRulesForSelectedProcess.
	nameAndRules second
		ifFalse: [self inform: 'Nope, won''t suspend ' , nameAndRules first.
			^ self].
	self class suspendProcess: selectedProcess.
	self updateProcessList
]

{ #category : #'process actions' }
ProcessBrowser >> terminateProcess [
	| nameAndRules |
	nameAndRules := self nameAndRulesForSelectedProcess.
	nameAndRules second
		ifFalse: [self inform: 'Nope, won''t kill ' , nameAndRules first.
			^ self].
	self class terminateProcess: selectedProcess.	
	self updateProcessList
]

{ #category : #accessing }
ProcessBrowser >> text [
	^methodText
]

{ #category : #updating }
ProcessBrowser >> toggleAutoUpdate [
	self isAutoUpdating
		ifTrue: [ self stopAutoUpdate ]
		ifFalse: [ self startAutoUpdate ].

]

{ #category : #'process list' }
ProcessBrowser >> updateProcessList [
	| oldSelectedProcess newIndex now |
	now := Time millisecondClockValue.
	now - lastUpdate < 500
		ifTrue: [^ self].
	"Don't update too fast"
	lastUpdate := now.
	oldSelectedProcess := selectedProcess.
	processList := selectedProcess := selectedSelector := nil.
	Smalltalk garbageCollectMost.
	"lose defunct processes"

	processList := Process allSubInstances
				reject: [:each | each isTerminated].
	processList := processList
				sort: [:a :b | a priority >= b priority].
	processList := WeakArray withAll: processList.
	newIndex := processList
				indexOf: oldSelectedProcess
				ifAbsent: [0].
	self changed: #processNameList.
	self processListIndex: newIndex
]

{ #category : #'stack list' }
ProcessBrowser >> updateStackList [
	self updateStackList: 20
]

{ #category : #'stack list' }
ProcessBrowser >> updateStackList: depth [ 
	| suspendedContext oldHighlight |
	selectedProcess
		ifNil: [^ self changeStackListTo: nil].
	(stackList notNil and: [ stackListIndex > 0 ])
		ifTrue: [oldHighlight := stackList at: stackListIndex].
	selectedProcess == Processor activeProcess
		ifTrue: [self
				changeStackListTo: (thisContext stackOfSize: depth)]
		ifFalse: [suspendedContext := selectedProcess suspendedContext.
			suspendedContext
				ifNil: [self changeStackListTo: nil]
				ifNotNil: [self
						changeStackListTo: (suspendedContext stackOfSize: depth)]].
	self
		stackListIndex: (stackList
				ifNil: [0]
				ifNotNil: [stackList indexOf: oldHighlight])
]

{ #category : #'process actions' }
ProcessBrowser >> wasProcessSuspendedByProcessBrowser: aProcess [
	^self class suspendedProcesses includesKey: aProcess
]

{ #category : #initialization }
ProcessBrowser >> windowIsClosing [
	startedCPUWatcher ifTrue: [ CPUWatcher stopMonitoring ]
]
