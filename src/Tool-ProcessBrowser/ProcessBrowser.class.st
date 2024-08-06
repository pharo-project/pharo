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
	#name : 'ProcessBrowser',
	#superclass : 'Model',
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
		'keyEventsDict',
		'textModel',
		'cpuWatcherMonitoring'
	],
	#classVars : [
		'SuspendedProcesses',
		'WellKnownProcesses'
	],
	#category : 'Tool-ProcessBrowser',
	#package : 'Tool-ProcessBrowser'
}

{ #category : 'class initialization' }
ProcessBrowser class >> cleanUp [
 	"Remove terminated processes from my suspended list"
	<script>

	self suspendedProcesses keysDo: [:ea |
		ea isTerminated ifTrue: [self suspendedProcesses removeKey: ea]]
]

{ #category : 'process control' }
ProcessBrowser class >> debugProcess: aProcess [
	self resumeProcess: aProcess.
	aProcess debugWithTitle: 'Interrupted from the Process Browser'
]

{ #category : 'CPU utilization' }
ProcessBrowser class >> dumpPigStackOn: aStream [
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
				aStream cr]]
]

{ #category : 'CPU utilization' }
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

{ #category : 'CPU utilization' }
ProcessBrowser class >> dumpTallyOnTranscript: tally [
	"tally is from ProcessorScheduler>>tallyCPUUsageFor:
	Dumps lines with percentage of time, hash of process, and a friendly name"

	self dumpTally: tally on: Smalltalk tools transcript.
	Smalltalk tools transcript flush.
]

{ #category : 'class initialization' }
ProcessBrowser class >> initialize [
	SuspendedProcesses ifNil: [ SuspendedProcesses := IdentityDictionary new ].
	self registerWellKnownProcesses
]

{ #category : 'process control' }
ProcessBrowser class >> isUIProcess: aProcess [
	^ aProcess == MorphicUIManager uiProcess
]

{ #category : 'world menu' }
ProcessBrowser class >> menuCommandOn: aBuilder [
	<worldMenu>
	(aBuilder item: #'Process Browser')
		parent: #SystemTools;
		order: 3;
		action:[ self open ];
		help: 'Provides a view of all of the processes (threads) executing in Smalltalk.';
		iconName: self taskbarIconName
]

{ #category : 'process control' }
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

{ #category : 'instance creation' }
ProcessBrowser class >> open [
	"Create and schedule a ProcessBrowser."
	<script>

	Smalltalk garbageCollect.
	^ self new build
]

{ #category : 'settings' }
ProcessBrowser class >> processBrowserSettingsOn: aBuilder [

	<systemsettings>
	(aBuilder group: #processBrowser)
		label: 'Process browser';
		parent: #tools;
		description: 'Process browser settings';
		with: [
			(aBuilder setting: #cpuWatcherEnabled)
				label: 'Process browser monitors CPU usage';
				target: CPUWatcher;
				default: false;
				description: 'If true, Pharo processes will be monitored for CPU usage. If they take too much CPU, you will get a notification menu that will allow you to debug, resume, or terminate the process' .

			(aBuilder setting: #pigFinding)
				label: 'Find the most CPU demanding processes';
				target: CPUWatcher;
				default: false;
				description: 'Try to identify CPU demanding processes during CPU monitoring and offer their termination'  ]
]

{ #category : 'tools registry' }
ProcessBrowser class >> registerToolsOn: registry [
	"Add ourselves to registry. See [Smalltalk tools]"
	registry register: self as: #processBrowser
]

{ #category : 'process control' }
ProcessBrowser class >> registerWellKnownProcess: aBlockForProcess label: nickname allowStop: allowStop allowDebug: allowDebug [
	"Add an entry to the registry of well known processes. aBlockForProcess
	evaluates to a known process to be identified by nickname, and allowStop
	and allowDebug are flags controlling allowable actions for this process
	in the browser."

	WellKnownProcesses add: aBlockForProcess->[{nickname . allowStop . allowDebug}]
]

{ #category : 'private - initialization' }
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
		registerWellKnownProcess: [ FinalizationProcess runningFinalizationProcess ]
		label: nil
		allowStop: false
		allowDebug: false.
	self
		registerWellKnownProcess: [ Processor backgroundProcess ]
		label: nil
		allowStop: false
		allowDebug: false.
	self
		registerWellKnownProcess: [ MorphicUIManager uiProcess ]
		label: nil
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

{ #category : 'cleanup' }
ProcessBrowser class >> restartMethods [

	self registerWellKnownProcesses
]

{ #category : 'process control' }
ProcessBrowser class >> resumeProcess: aProcess [
	| priority |
	priority := self suspendedProcesses
				removeKey: aProcess
				ifAbsent: [aProcess priority].
	aProcess priority: priority.
	aProcess suspendedContext ifNotNil: [ aProcess resume ]
]

{ #category : 'process control' }
ProcessBrowser class >> setProcess: aProcess toPriority: priority [
	| oldPriority |
	oldPriority := self suspendedProcesses at: aProcess ifAbsent: [ ].
	oldPriority ifNotNil: [ self suspendedProcesses at: aProcess put: priority ].
	aProcess priority: priority.
	^oldPriority
]

{ #category : 'process control' }
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
	aProcess priority: priority
]

{ #category : 'process control' }
ProcessBrowser class >> suspendedProcesses [
	"Answer a collection of processes that my instances have suspended.
	This is so that they don't get garbage collected."
	^ SuspendedProcesses
		ifNil: [ SuspendedProcesses := IdentityDictionary new ]
]

{ #category : 'CPU utilization' }
ProcessBrowser class >> tallyCPUUsageFor: seconds [
	"Compute CPU usage using a 10-msec sample for the given number of seconds,
	then dump the usage statistics on the Transcript. The UI is free to continue, meanwhile"
	"ProcessBrowser tallyCPUUsageFor: 10"
	^self tallyCPUUsageFor: seconds every: 10
]

{ #category : 'CPU utilization' }
ProcessBrowser class >> tallyCPUUsageFor: seconds every: msec [
	"Compute CPU usage using a msec millisecond sample for the given number of seconds,
	then dump the usage statistics on the Transcript. The UI is free to continue, meanwhile"

	"ProcessBrowser tallyCPUUsageFor: 10 every: 100"

	| promise |
	promise := Processor tallyCPUUsageFor: seconds every: msec.

	[
	| tally |
	tally := promise value.
	MorphicUIManager new defer: [ self dumpTallyOnTranscript: tally ] ]
		fork
]

{ #category : 'icons' }
ProcessBrowser class >> taskbarIconName [
	"Answer the icon for the receiver in a task bar."

	^#processBrowser
]

{ #category : 'process control' }
ProcessBrowser class >> terminateProcess: aProcess [
	aProcess ifNotNil: [
		self suspendedProcesses
			removeKey: aProcess
			ifAbsent: [].
		aProcess terminate
	]
]

{ #category : 'stack list' }
ProcessBrowser >> browseContext [

	selectedContext ifNil: [ ^ self ].
	self tools browser
		openOnClass: self selectedClass
		selector: self selectedSelector
]

{ #category : 'views' }
ProcessBrowser >> build [
	| window  scrolledText |
	window := (SystemWindow labelled: 'later') model: self.
	self deferredMessageRecipient: window currentWorld.
	window
		addMorph: ((PluggableListMorph
				on: self
				list: #processNameList
				selected: #processListIndex
				changeSelected: #processListIndex:
				menu: #menuProcessList:
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
	scrolledText := textModel newScrolledText beForSmalltalkScripting; yourself.
	window
		addMorph:  scrolledText
		frame: (0 @ 0.5 corner: 1 @ 1).
	window setUpdatablePanesFrom: #(#processNameList #stackNameList ).
	(window setLabel: 'Process Browser') openInWorld.
	self mayBeStartCPUWatcher.
	^ window
]

{ #category : 'process actions' }
ProcessBrowser >> changePriority [
	| str newPriority nameAndRules |
	nameAndRules := self nameAndRulesForSelectedProcess.
	nameAndRules third
		ifFalse: [self inform: 'Nope, won''t change priority of ' , nameAndRules first.
			^ self].
	str := self morphicUIManager
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

{ #category : 'stack list' }
ProcessBrowser >> changeStackListTo: aCollection [

        stackList := aCollection.
        self changed: #stackNameList.
        self stackListIndex: 0.
	textModel setText: self methodText.
]

{ #category : 'process actions' }
ProcessBrowser >> debugProcess [
	| nameAndRules |
	nameAndRules := self nameAndRulesForSelectedProcess.
	nameAndRules third
		ifFalse: [self inform: 'Nope, won''t debug ' , nameAndRules first.
			^ self].
	self class debugProcess: selectedProcess
]

{ #category : 'accessing' }
ProcessBrowser >> deferredMessageRecipient: anObject [
	deferredMessageRecipient := anObject
]

{ #category : 'accessing' }
ProcessBrowser >> doItContext [
	^selectedContext
]

{ #category : 'accessing' }
ProcessBrowser >> doItReceiver [
	^selectedContext ifNil: [ selectedProcess ] ifNotNil: [ selectedContext receiver ]
]

{ #category : 'initialization' }
ProcessBrowser >> ensureCPUWatcherMonitoring [

	self startCPUWatcher.
	^ true
]

{ #category : 'process list' }
ProcessBrowser >> findContext [
	| initialProcessIndex initialStackIndex found |
	initialProcessIndex := self processListIndex.
	initialStackIndex := self stackListIndex.
	searchString := self morphicUIManager
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

{ #category : 'views' }
ProcessBrowser >> hasView [
	^self dependents isEmptyOrNil not
]

{ #category : 'initialization' }
ProcessBrowser >> initialize [
	super initialize.
	self class registerWellKnownProcesses.
	methodText := ''.
	stackListIndex := 0.
	searchString := ''.
	lastUpdate := 0.
	textModel :=  RubScrolledTextModel new interactionModel: self; yourself.
	cpuWatcherMonitoring := CPUWatcher cpuWatcherEnabled and: [ self ensureCPUWatcherMonitoring ].
	self updateProcessList; processListIndex: 1
]

{ #category : 'stack list' }
ProcessBrowser >> inspectContext [
	selectedContext inspect
]

{ #category : 'process actions' }
ProcessBrowser >> inspectPointers [

	| tc pointers |
	selectedProcess ifNil: [ ^ self ].
	tc := thisContext.
	pointers := selectedProcess pointersToExcept: {
			            self processList.
			            tc.
			            self }.
	pointers ifEmpty: [ ^ self ].
	pointers inspectWithLabel: 'Objects pointing to ' , selectedProcess browserPrintString
]

{ #category : 'process list' }
ProcessBrowser >> inspectProcess [
	selectedProcess inspect
]

{ #category : 'stack list' }
ProcessBrowser >> inspectReceiver [

	selectedContext ifNotNil: [ selectedContext receiver inspect ]
]

{ #category : 'updating' }
ProcessBrowser >> isAutoUpdating [

	^ autoUpdateProcess isNotNil and: [ autoUpdateProcess isSuspended  not ]
]

{ #category : 'updating' }
ProcessBrowser >> isAutoUpdatingPaused [

	^ autoUpdateProcess isNotNil and: [ autoUpdateProcess isSuspended ]
]

{ #category : 'testing' }
ProcessBrowser >> isScripting [
	^self doItContext belongsToDoIt
]

{ #category : 'process list' }
ProcessBrowser >> keyEventsDict [

	^ keyEventsDict ifNil: [
		keyEventsDict := Dictionary newFromPairs:  #(
			$i #inspectProcess
			$P #inspectPointers
			$t #terminateProcess
			$r #resumeProcess
			$s #suspendProcess
			$d #debugProcess
			$p #changePriority
			$f #findContext
			$g #nextContext
			$a #toggleAutoUpdate
			$u #updateProcessList
			$S #signalSemaphore
			)
		]
]

{ #category : 'initialization' }
ProcessBrowser >> mayBeStartCPUWatcher [
	cpuWatcherMonitoring ifTrue: [ self setUpdateCallbackAfter: 7 ]
]

{ #category : 'menu' }
ProcessBrowser >> menuProcessList: aMenuMorph [

	selectedProcess
		ifNotNil: [
			| nameAndRules |
			nameAndRules := self nameAndRulesForSelectedProcess.
			aMenuMorph addMenuItem: ((ToggleMenuItemMorph new contents: #'Inspect')
				keyText: 'i';
				selector: #inspectProcess;
				target: self;
				yourself).
			aMenuMorph addMenuItem: ((ToggleMenuItemMorph new contents: #'Inspect Pointers')
				keyText: 'P';
				selector: #inspectPointers;
				target: self;
				yourself).

			nameAndRules second
				ifTrue: [
					aMenuMorph addMenuItem: ((ToggleMenuItemMorph new contents: #'Terminate')
						keyText: 't';
						selector: #terminateProcess;
						target: self;
						yourself).

					selectedProcess isSuspended
						ifTrue: [
							aMenuMorph addMenuItem: ((ToggleMenuItemMorph new contents: #'Resume')
								keyText: 'r';
								selector: #resumeProcess;
								target: self;
								yourself). ]
						ifFalse: [
							aMenuMorph addMenuItem: ((ToggleMenuItemMorph new contents: #'Suspend')
								keyText: 's';
								selector: #suspendProcess;
								target: self;
								yourself). ] ].

			nameAndRules third
				ifTrue: [
					aMenuMorph addMenuItem: ((ToggleMenuItemMorph new contents: #'Change priority')
						keyText: 'p';
						selector: #changePriority;
						target: self;
						yourself).
					aMenuMorph addMenuItem: ((ToggleMenuItemMorph new contents: #'Debug')
						keyText: 'd';
						selector: #debugProcess;
						target: self;
						yourself) ].

			(selectedProcess suspendingList isKindOf: Semaphore)
				ifTrue: [
					aMenuMorph addMenuItem: ((ToggleMenuItemMorph new contents: #'Signal Semaphore')
						keyText: 'S';
						selector: #signalSemaphore;
						target: self;
						yourself). ].

			aMenuMorph addMenuItem: (MenuLineMorph new)].

	aMenuMorph addMenuItem: ((ToggleMenuItemMorph new contents: #'Find context...')
		keyText: 'f';
		selector: #findContext;
		target: self;
		yourself).
	aMenuMorph addMenuItem: ((ToggleMenuItemMorph new contents: #'Find again')
		keyText: 'g';
		selector: #nextContext;
		target: self;
		yourself).

	aMenuMorph addMenuItem: (MenuLineMorph new).

	aMenuMorph addMenuItem: ((ToggleMenuItemMorph new contents: (self isAutoUpdating
			ifTrue: [ #'Turn off auto-update' ]
			ifFalse: [ #'Turn on auto-update' ]))
		keyText: 'a';
		selector: #toggleAutoUpdate;
		target: self;
		yourself).

	aMenuMorph addMenuItem: ((ToggleMenuItemMorph new contents: #'Update list')
		keyText: 'u';
		selector: #updateProcessList;
		target: self;
		yourself).

	Smalltalk globals
		at: #CPUWatcher
		ifPresent: [ :pw |
			aMenuMorph addMenuItem: (MenuLineMorph new).
			pw isMonitoring
				ifTrue: [
					aMenuMorph addMenuItem: ((ToggleMenuItemMorph new contents: #'Stop CPUWatcher')
						selector: #stopCPUWatcher;
						target: self;
						yourself) ]
				ifFalse: [
					aMenuMorph addMenuItem: ((ToggleMenuItemMorph new contents: #'Start CPUWatcher')
						selector: #startCPUWatcher;
						target: self;
						yourself) ] ].

	^ aMenuMorph
]

{ #category : 'menu' }
ProcessBrowser >> menuStackList: aMenuMorph [

	aMenuMorph addMenuItem: ((ToggleMenuItemMorph new contents: #'Inspect context')
		keyText: 'c';
		selector: #inspectContext;
		target: self;
		yourself).

	aMenuMorph addMenuItem: MenuLineMorph new.

	aMenuMorph addMenuItem:((ToggleMenuItemMorph new contents: #'Inspect receiver')
		keyText: 'i';
		selector: #inspectReceiver;
		target: self;
		yourself).

	aMenuMorph addMenuItem: MenuLineMorph new.

	aMenuMorph addMenuItem:((ToggleMenuItemMorph new contents: #'Browse')
		keyText: 'b';
		selector: #browseContext;
		target: self;
		yourself)
]

{ #category : 'accessing' }
ProcessBrowser >> methodText [
	^ methodText ifNil: [methodText := selectedContext
						ifNil: ['']
						ifNotNil: [| pcRange |
							methodText := [ selectedContext sourceCode ]
								onErrorDo: [ 'error getting method text' ].
							pcRange := self pcRange.
							methodText asText
								addAttribute: TextColor red
								from: pcRange first
								to: pcRange last;
								addAttribute: TextEmphasis bold
								from: pcRange first
								to: pcRange last]]
]

{ #category : 'process actions' }
ProcessBrowser >> morphicUIManager [

	^ MorphicUIManager new
]

{ #category : 'process actions' }
ProcessBrowser >> nameAndRulesFor: aProcess [
	"Answer a nickname and two flags: allow-stop, and allow-debug"
	aProcess == autoUpdateProcess ifTrue: [ ^{'my auto-update process'. true. true} ].
	^self class nameAndRulesFor: aProcess
]

{ #category : 'process actions' }
ProcessBrowser >> nameAndRulesForSelectedProcess [
	"Answer a nickname and two flags: allow-stop, and allow-debug"
	^self nameAndRulesFor: selectedProcess
]

{ #category : 'process list' }
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

{ #category : 'process list' }
ProcessBrowser >> notify: errorString at: location in: aStream [
	"A syntax error happened when I was trying to highlight my pc.
	Raise a signal so that it can be ignored."
	Warning signal: 'syntax error'
]

{ #category : 'stack list' }
ProcessBrowser >> pcRange [
	"Answer the indices in the source code for the method corresponding to
	the selected context's program counter value."
	(selectedContext isNil or: [methodText isEmptyOrNil])
		ifTrue: [^ 1 to: 0].
	^selectedContext pcRangeContextIsActive: (stackListIndex = 1)
]

{ #category : 'process list' }
ProcessBrowser >> prettyNameForProcess: aProcess [
	| nameAndRules |
	aProcess ifNil: [ ^'<nil>' ].
	nameAndRules := self nameAndRulesFor: aProcess.
	^ aProcess browserPrintStringWith: nameAndRules first
]

{ #category : 'accessing' }
ProcessBrowser >> processList [
	^ processList
]

{ #category : 'accessing' }
ProcessBrowser >> processListIndex [
	^ processListIndex
]

{ #category : 'accessing' }
ProcessBrowser >> processListIndex: index [
	processListIndex := index.
	selectedProcess := processList
				at: index
				ifAbsent: [].
	self updateStackList.
	self changed: #processListIndex
]

{ #category : 'process list' }
ProcessBrowser >> processListKey: aKey from: aView [
	self perform: (self keyEventsDict at: aKey ifAbsent: [ ^ self ])
]

{ #category : 'process list' }
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

{ #category : 'process actions' }
ProcessBrowser >> resumeProcess [
	selectedProcess
		ifNil: [^ self].
	self class resumeProcess: selectedProcess.
	self updateProcessList
]

{ #category : 'accessing' }
ProcessBrowser >> selectedClass [
	"Answer the class in which the currently selected context's method was  found."
	^ selectedClass
		ifNil: [ selectedClass := selectedContext receiver
				ifNil: [ selectedSelector := selectedContext method selector.
					   selectedContext method methodClass ]
				ifNotNil: [ selectedContext methodClass ]]
]

{ #category : 'accessing' }
ProcessBrowser >> selectedClassOrMetaClass [
	^self doItReceiver class
]

{ #category : 'accessing' }
ProcessBrowser >> selectedMethod [
	^selectedContext ifNotNil: [ selectedContext method ]
]

{ #category : 'accessing' }
ProcessBrowser >> selectedProcess [
	^selectedProcess
]

{ #category : 'accessing' }
ProcessBrowser >> selectedSelector [
	"Answer the class in which the currently selected context's method was
	found."
	^ selectedSelector
		ifNil: [selectedSelector := selectedContext receiver
				ifNil: [selectedClass := selectedContext method methodClass
					   selectedContext method selector]
				ifNotNil: [selectedContext selector]]
]

{ #category : 'updating' }
ProcessBrowser >> setUpdateCallbackAfter: seconds [
	deferredMessageRecipient
		ifNotNil: [
			| d |
			d := Delay forSeconds: seconds.
			[ d wait.
			d := nil.
			deferredMessageRecipient defer: [ self updateProcessList ] ] fork ]
]

{ #category : 'updating' }
ProcessBrowser >> shoutAboutToStyle: aPluggableShoutMorphOrView [

	selectedContext ifNil: [^false].
	aPluggableShoutMorphOrView classOrMetaClass: self selectedClass.
	^ true
]

{ #category : 'process actions' }
ProcessBrowser >> signalSemaphore [
	(selectedProcess suspendingList isKindOf: Semaphore)
		ifFalse: [^ self].
	[selectedProcess suspendingList signal] fork.
	(Delay forMilliseconds: 300) wait.
	"Hate to make the UI wait, but it's convenient..."
	self updateProcessList
]

{ #category : 'accessing' }
ProcessBrowser >> stackList [
	^ stackList
]

{ #category : 'accessing' }
ProcessBrowser >> stackListIndex [
	^ stackListIndex
]

{ #category : 'accessing' }
ProcessBrowser >> stackListIndex: index [
	stackListIndex := index.
	selectedContext := (stackList isNotNil
						and: [index > 0]) ifTrue:
							[stackList at: index ifAbsent: []].
	selectedClass := nil.
	selectedSelector := nil.
	methodText := nil.
	textModel setText: self methodText
]

{ #category : 'views' }
ProcessBrowser >> stackListKey: aKey from: aView [
	aKey = $c ifTrue: [ self inspectContext].
	aKey = $i ifTrue: [ self inspectReceiver].
	aKey = $b ifTrue: [ self browseContext]
]

{ #category : 'stack list' }
ProcessBrowser >> stackListMenu: aMenu [
	selectedContext ifNil: [^ aMenu].
	self menuStackList: aMenu.
	^ aMenu
]

{ #category : 'stack list' }
ProcessBrowser >> stackNameList [
	^ stackList
		ifNil: [#()]
		ifNotNil: [stackList collect: [:each | each asString]]
]

{ #category : 'updating' }
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

{ #category : 'initialization' }
ProcessBrowser >> startCPUWatcher [

	CPUWatcher isMonitoring ifTrue: [ ^ self ].

	CPUWatcher startMonitoringPeriod: 5 rate: 100 threshold: 0.85.
	self setUpdateCallbackAfter: 7
]

{ #category : 'updating' }
ProcessBrowser >> stopAutoUpdate [
	autoUpdateProcess ifNotNil: [
		autoUpdateProcess terminate.
		autoUpdateProcess := nil].
	self updateProcessList
]

{ #category : 'initialization' }
ProcessBrowser >> stopCPUWatcher [

	CPUWatcher stopMonitoring.
	self updateProcessList.
	cpuWatcherMonitoring := false	"so a manual restart won't be killed later"
]

{ #category : 'process actions' }
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

{ #category : 'process actions' }
ProcessBrowser >> terminateProcess [
	| nameAndRules |
	nameAndRules := self nameAndRulesForSelectedProcess.
	nameAndRules second
		ifFalse: [self inform: 'Nope, won''t kill ' , nameAndRules first.
			^ self].
	self class terminateProcess: selectedProcess.
	self updateProcessList
]

{ #category : 'accessing' }
ProcessBrowser >> text [
	^methodText
]

{ #category : 'updating' }
ProcessBrowser >> toggleAutoUpdate [
	self isAutoUpdating
		ifTrue: [ self stopAutoUpdate ]
		ifFalse: [ self startAutoUpdate ]
]

{ #category : 'stack list' }
ProcessBrowser >> tools [

	^ Smalltalk tools
]

{ #category : 'process list' }
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

{ #category : 'stack list' }
ProcessBrowser >> updateStackList [

	| suspendedContext oldHighlight |
	selectedProcess ifNil: [ ^ self changeStackListTo: nil ].

	(stackList isNotNil and: [ stackListIndex > 0 ]) ifTrue: [ oldHighlight := stackList at: stackListIndex ].
	selectedProcess == Processor activeProcess
		ifTrue: [ self changeStackListTo: thisContext stack ]
		ifFalse: [
			suspendedContext := selectedProcess suspendedContext.
			suspendedContext
				ifNil: [ self changeStackListTo: nil ]
				ifNotNil: [ self changeStackListTo: suspendedContext stack ] ].
	self stackListIndex: (stackList
			 ifNil: [ 0 ]
			 ifNotNil: [ stackList indexOf: oldHighlight ])
]

{ #category : 'initialization' }
ProcessBrowser >> windowIsClosing [

	cpuWatcherMonitoring ifTrue: [ CPUWatcher stopMonitoring ]
]
