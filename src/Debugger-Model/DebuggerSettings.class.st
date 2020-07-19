"
General debugger settings with default values for TDebugger users
"
Class {
	#name : #DebuggerSettings,
	#superclass : #Object,
	#category : #'Debugger-Model-Core'
}

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
DebuggerSettings class >> systemDebuggers [
	^ TDebugger users reject: [ :class | class isTestCase ]
]
