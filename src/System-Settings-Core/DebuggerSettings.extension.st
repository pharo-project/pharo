Extension { #name : 'DebuggerSettings' }

{ #category : '*System-Settings-Core' }
DebuggerSettings class >> debuggerRankSettingsOn: aBuilder [

	<systemsettings>
	(aBuilder group: #debuggers)
		label: 'Debuggers';
		parentName: #debugging.
	self systemDebuggers do: [ :debuggerClass |
			(aBuilder group: debuggerClass name asSymbol)
				label: debuggerClass name;
				parentName: #debuggers;
				with: [
						(aBuilder pickOne: #rank)
							label: 'Priority';
							target: debuggerClass;
							defaultValue: debuggerClass defaultDebuggerRank;
							domainValues: (1 to: 10);
							description:
								'Debugger priority: a debugger with a high priority will be chosen for debugging over a debugger with a low priority.
Lowest priority: 1. Highest priority: 10'.
						(aBuilder setting: #availableAutomatically)
							label: 'Available';
							target: debuggerClass;
							defaultValue: self defaultAvailability;
							description: '(De)activates the debugger. A deactivated debugger will not be used by the system.' ] ]
]

{ #category : '*System-Settings-Core' }
DebuggerSettings class >> generalDebuggingSettingsOn: aBuilder [

	<systemsettings>
	(aBuilder group: #debugging)
		label: 'Debugging';
		parentName: #tools;
		description: 'All Debugger settings';
		with: [
			(aBuilder group: #deprecationHandling)
				label: 'Deprecation handling';
				description: 'How deprecation are handled';
				with: [ "A pick-one setting does not take the target of its group, so each names it"
					(aBuilder pickOne: #reaction)
						label: 'Reaction';
						target: Deprecation;
						domainValues: DeprecationReaction allSubclasses;
						defaultValue: Deprecation defaultReaction;
						description: 'What happens when a deprecated method runs. Every deprecation is recorded in the log anyway.'.
					(aBuilder pickOne: #rewriting)
						label: 'Rewrite the callers';
						target: Deprecation;
						domainValues: DeprecationRewriting allSubclasses;
						defaultValue: Deprecation defaultRewriting;
						description:
							'When a deprecation that has a rewriting rule rewrites the caller of the deprecated method. PHEP-0004 rewrites only in the last version of its compatibility period: 4 versions for kernel packages, 2 for the others.' ].

			(aBuilder setting: #logDebuggerStackToFile)
				label: 'Write message to debug log file when fall into debugger';
				target: DebugSession;
				defaultValue: false;
				description: 'If true, whenever you fall into a debugger a summary of its stack will be written to a file named'.


			(aBuilder setting: #logFileName)
				label: 'Log file name';
				target: Smalltalk;
				defaultValue: 'PharoDebug.log';
				description: 'A name of the file, which will be used for logging all errors and notifications' ]
]
