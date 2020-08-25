"
General debugger settings with default values for TDebugger users
"
Class {
	#name : #DebuggerSettings,
	#superclass : #Object,
	#category : #'Debugger-Model-Core'
}

{ #category : #settings }
DebuggerSettings class >> availableDebuggers [

	^ (self systemDebuggers select: [ :dbgClass | 
		   dbgClass availableAutomatically ]) asOrderedCollection
]

{ #category : #settings }
DebuggerSettings class >> debuggerRankSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #debuggers)
		label: 'Debuggers';
		parent: #debugging.
	self systemDebuggers do: [ :debuggerClass | 
		(aBuilder group: debuggerClass name asSymbol)
			label: debuggerClass name;
			parent: #debuggers;
			with: [ 
				(aBuilder pickOne: #rank)
					label: 'Priority';
					target: debuggerClass;
					default: self defaultDebuggerRank;
					domainValues: (1 to: 10);
					description:
						'Debugger priority: a debugger with a high priority will be chosen for debugging over a debugger with a low priority. 
Lowest priority: 1. Highest priority: 10'.
				(aBuilder setting: #availableAutomatically)
					label: 'Available';
					target: debuggerClass;
					default: self defaultAvailability;
					description:
						'(De)activates the debugger. A deactivated debugger will not be used by the system.' ] ]
]

{ #category : #accessing }
DebuggerSettings class >> defaultAvailability [
	^true
]

{ #category : #accessing }
DebuggerSettings class >> defaultDebuggerRank [
	^10
]

{ #category : #settings }
DebuggerSettings class >> generalDebuggingSettingsOn: aBuilder [

	<systemsettings>
	(aBuilder group: #debugging)
		label: 'Debugging';
		parent: #tools;
		description: 'All Debugger settings';
		with: [ 
			(aBuilder group: #deprecationHandling)
				label: 'Deprecation handling';
				description: 'How deprecation are handled';
				target: Deprecation;
				with: [ 
					(aBuilder setting: #activateTransformations)
						label:
							'Performs automatic transformations on deprecated methods';
						default: true;
						description:
							'If true, then the caller will be rewritten for each deprecated method invocation'.
					(aBuilder setting: #raiseWarning)
						label: 'Raise a blocking dialog';
						default: true;
						description:
							'If true, then a dialog is popup for each deprecated method invocation'.
					(aBuilder setting: #showWarning)
						label: 'Transcript message';
						default: true;
						description:
							'If true, then a message is send to the Transcript for each deprecated method invocation' ].

			(aBuilder setting: #cmdDotEnabled)
				label: 'Enable cmd-dot interrupt key';
				target: UserInterruptHandler;
				default: true;
				description:
					'If true, it allows the user to stop currently running process by opening-up a debugger'.


			(aBuilder setting: #logDebuggerStackToFile)
				label: 'Write message to debug log file when fall into debugger';
				target: DebugSession;
				default: true;
				description:
					'If true, whenever you fall into a debugger a summary of its stack will be written to a file named'.


			(aBuilder setting: #logFileName)
				label: 'Log file name';
				target: Smalltalk;
				default: 'PharoDebug.log';
				description:
					'A name of the file, which will be used for logging all errors and notifications' ]
]

{ #category : #settings }
DebuggerSettings class >> systemDebuggers [

	^ (TDebugger users flatCollect: [ :class | class withAllSubclasses ]) 
		  reject: [ :class | class isTestCase ]
]
