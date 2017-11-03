"
Settings for debugging
"
Class {
	#name : #DebugSystemSettings,
	#superclass : #Object,
	#category : #DebuggerModel
}

{ #category : #'private - settings' }
DebugSystemSettings class >> addDebugFilterSessingsOn: aBuilder [
	| stackWidgetClass |
	
	stackWidgetClass := Smalltalk tools debugger stackWidgetClass.
	(aBuilder group: #BasicFilters)
				label: 'Basic Filters';
				description: 'default filtering configurations';
				target: Smalltalk tools debugger;
				with: [
					(aBuilder setting: #filterCommonMessageSends)
				label: 'Filter out common message sends';
				target: Smalltalk tools debugger;
				description: 'When true, filter out uninteresting message sends in the Debugger view while debugging';
				with: [ 
					(aBuilder setting: #filterDoItSelectors)
					label: 'Filter out doIt sends';
					target: stackWidgetClass.
					
					(aBuilder setting: #filterNilSelectors)
					label: 'Filter out nil message sends';
					target: stackWidgetClass.
					
					(aBuilder setting: #filterKernelClasses)
					label: 'Filter out kernel classes message sends';
					target: stackWidgetClass.
					
					(aBuilder setting: #filterLinkSelectors)
					label: 'Filter out link and breakpoint message sends';
					target: stackWidgetClass.
					
					 ]
				].
]

{ #category : #settings }
DebugSystemSettings class >> debugSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #debugging)
		label: 'Debugging';
		parent: #tools;
		description: 'All Debugger settings';
		with: [
			(aBuilder group: #deprecationHandling)
				label: 'Deprecation handling';
				description: 'How deprecation are handled' ;
				target: Deprecation;
				with: [
					(aBuilder setting: #raiseWarning)
						label: 'Raise a blocking dialog' ;
						description: 'If true, then a dialog is popup for each deprecated method invocation' .
					(aBuilder setting: #showWarning)
						label: 'Transcript message';
						description: 'If true, then a message is send to the Transcript for each deprecated method invocation'].
			
			(aBuilder setting: #cmdDotEnabled)
				label: 'Enable cmd-dot interrupt key';
				target: UserInterruptHandler;
				description: 'If true, it allows the user to stop currently running process by opening-up a debugger'.
			
			(aBuilder setting: #cpuWatcherEnabled)
				label: 'Process browser monitors CPU usage';
				target: CPUWatcher;
				description: 'If true, Pharo processes will be monitored for CPU usage. If they take too much CPU, you will get a notification menu that will allow you to debug, resume, or terminate the process' .
			
			(aBuilder setting: #debugShowDamage)
				label: 'Flash damaged morphic region';
				target: WorldState;
				description: 'If true, every changed region of the morphic display will be flashed black before updating.' .
			
			(aBuilder setting: #logDebuggerStackToFile)
				label: 'Write message to debug log file when fall into debugger';
				target: DebugSession;
				description: 'If true, whenever you fall into a debugger a summary of its stack will be written to a file named'.
			
			"(aBuilder setting: #restartAlsoProceeds)
				label: 'Restart also proceeds' ;
				target: Debugger;
				description: 'If this preference is set, the debugger''s restart button and menu item will also proceed. If the preference is not set, the selected context will just be reset to its initial condition, so you may step through it again.' ."
				
			(aBuilder setting: #alwaysOpenFullDebugger)
				label: 'Directly open the full Debugger';
				target: Smalltalk tools debugger;
				description: 'When true, always directly open the full Debugger view when debugging instead of showing only a small popup' .
			
			self addDebugFilterSessingsOn: aBuilder.
				
			(aBuilder setting: #logFileName)
				label: 'Log file name' ;
				target: Smalltalk;
				description: 'A name of the file, which will be used for logging all errors and notifications' 
		]
]
