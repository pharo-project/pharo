Extension { #name : #WorldState }

{ #category : #'*Morphic-Base' }
WorldState >> addAlarm: aSelector withArguments: argArray for: aTarget at: scheduledTime [
	"Add a new alarm with the given set of parameters"
	self alarms add: 
		(MorphicAlarm 
			scheduledAt: scheduledTime
			receiver: aTarget
			selector: aSelector
			arguments: argArray).
]

{ #category : #'*Morphic-Base' }
WorldState class >> debugOn: aBuilder [
	<worldMenu>
	(aBuilder item: #Debugging)
		order: 3.5;
		iconName: #smallDebug;
		with: [ (aBuilder item: #'Remove all Breakpoints')
				order: 0;
				help: 'Remove all the breakpoints of the image.';
				action: [ Breakpoint removeAll ].
			(aBuilder item: #'Enable all break/inspect once')
				order: 0;
				help: 'Reset the break/inspect once. Once restarted, the first break once encountered will stop the execution.';
				action: [ Halt resetOnce ].
			(aBuilder item: #'Reset Counters')
				order: 0;
				help: 'Reset the counters on the executions counters.';
				action: [ ExecutionCounter resetAll ] ]
]

{ #category : #'*Morphic-Base' }
WorldState >> menuBuilder [
	^ menuBuilder ifNil: [
		menuBuilder := (PragmaMenuBuilder pragmaKeyword: self discoveredMenuPragmaKeyword model: self)
				menuSpec;
				yourself
	]
]

{ #category : #'*Morphic-Base' }
WorldState class >> startMessageTally [
	(self confirm: 'MessageTally will start now,
and stop when the cursor goes
to the top of the screen' translated) 
		ifTrue: [TimeProfiler spyAllOn: [[World activeHand position y > 0] 
					whileTrue: [World doOneCycle]]]
]

{ #category : #'*Morphic-Base' }
WorldState >> startStepping: aMorph at: scheduledTime selector: aSelector arguments: args stepTime: stepTime [
	"Add the given morph to the step list. Do nothing if it is already being stepped."

	self stopStepping: aMorph selector: aSelector.
	self adjustWakeupTimesIfNecessary.
	stepList add:(
		StepMessage 
			scheduledAt: scheduledTime
			stepTime: stepTime
			receiver: aMorph
			selector: aSelector
			arguments: args)
]

{ #category : #'*Morphic-Base' }
WorldState class >> startThenBrowseMessageTally [
	"Tally only the UI process"
	
	(self confirm: 'MessageTally the UI process until the
mouse pointer goes to the top of the screen')
		ifTrue: [TimeProfiler
				onBlock: [[World activeHand position y > 10]
						whileTrue: [World doOneCycle]]]
]

{ #category : #'*Morphic-Base' }
WorldState class >> systemOn: aBuilder [
	<worldMenu>
	(aBuilder item: #System)
		order: 3.0;
		iconName: #smallConfigurationIcon;
		with: [ (aBuilder item: #'About...')
				order: 0;
				iconName: #smallLanguageIcon;
				help: 'Informations about this version of Pharo.';
				action: [ Smalltalk aboutThisSystem ].
			(aBuilder item: #'Start profiling all Processes')
				help: 'Profile all Pharo processes.';
				action: [ self startMessageTally ].
			(aBuilder item: #'Start profiling UI ')
				help: 'Profile the UI process of Pharo.';
				action: [ self startThenBrowseMessageTally ].
			(aBuilder item: #'Space left')
				help: 'Do a garbage collection, and report results about the to the user.';
				action: [ Smalltalk informSpaceLeftAfterGarbageCollection ].
			(aBuilder item: #'Do Image Cleanup')
				help: 'Clean out any caches and other state that should be flushed when trying to get an image into a pristine state.';
				action: [ Smalltalk cleanUp: true ];
				withSeparatorAfter.
			(aBuilder item: #'Start drawing again')
				help: 'Resume the drawing after a draw error of the world.';
				action: [ World resumeAfterDrawError ].
			(aBuilder item: #'Start stepping again')
				help: 'Resume stepping of Morph after an error has occured.';
				action: [ World resumeAfterStepError ];
				withSeparatorAfter.
			(aBuilder item: #'Restore display')
				help: 'Restore Morphic display.';
				action: [ World restoreMorphicDisplay ] ]
]

{ #category : #'*Morphic-Base' }
WorldState class >> windowsOn: aBuilder [
	<worldMenu>
	(aBuilder item: #Windows)
		order: 4.0;
		withSeparatorAfter;
		iconName: #smallWindowIcon;
		with: [ (aBuilder item: #'Collapse all windows')
				action: [ World collapseAll ];
				help: 'Reduce all open windows to collapsed forms that only show titles' translated.
			(aBuilder item: #'Expand all windows')
				iconName: #expandBoxIcon;
				action: [ World expandAll ];
				help: 'Expand all collapsed windows back to their expanded forms' translated.
			(aBuilder item: #'Fit all windows')
				action: [ World fitAllVisibleWindows ];
				help: 'Fit all open windows as visible in World' translated.
			(aBuilder item: #'Close all debuggers')
				help: 'Close all opened debugger in the image.';
				action: [ Smalltalk tools debugger closeAllDebuggers ].
			(aBuilder item: #'Send top window to back (\)')
				action: [ SystemWindow sendTopWindowToBack ];
				help: 'Make the topmost window become the backmost one, and activate the window just beneath it.' translated.
			(aBuilder item: #'Move windows onscreen')
				action: [ World bringWindowsFullOnscreen ];
				help: 'Make all windows fully visible on the screen' translated.
			(aBuilder item: #'Delete unchanged windows')
				action: [ World closeUnchangedWindows ];
				help: 'Deletes all windows that do not have unsaved text edits.' translated.
			(aBuilder item: #'Delete all windows discarding edits')
				action: [ World closeAllWindowsDiscardingChanges ];
				help: 'Deletes all windows even if they have unsaved text edits.' translated.
			(aBuilder item: #'Toggle full screen mode')
				action: [ Display toggleFullscreen ];
				help: 'Turn full screen mode on when it is off, off when it is on.' translated ]
]
