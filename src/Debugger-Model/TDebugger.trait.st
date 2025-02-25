"
Use me if you want to be recognized as a debugger.
I specify the interface that all debuggers must implement to be used as debuggers.
Conversely, classes recognized as debuggers are all able to start debugging from a given debug session.

The API that all debuggers will expose by using me is:

`availableAutomatically:` a boolean telling if the debugger is (de)activated.
`rank:` an integer giving a priority to the debugger (against other debuggers priorities). A high priority debugger will be preferably opened by the system rather than a low priority debugger.

`handlesContext: aContext`: returns a boolean, tells if the debugger can handle the given context (e.g., a SUnit debugger will be able to handle test cases contexts). By default, any context is handleable.

`debugSession: aDebugSession`: Explicit requirement that must be implemented by all debuggers.
This API method is the entry point to open a debugger on a DebugSession instance.
It is the preferred debugger opening entry point used by the system to open a debugger.
"
Trait {
	#name : 'TDebugger',
	#classInstVars : [
		'rank',
		'availableAutomatically'
	],
	#category : 'Debugger-Model-Core',
	#package : 'Debugger-Model',
	#tag : 'Core'
}

{ #category : 'accessing' }
TDebugger classSide >> availableAutomatically [
	^ availableAutomatically ifNil: [
		  availableAutomatically := DebuggerSettings defaultAvailability ]
]

{ #category : 'accessing' }
TDebugger classSide >> availableAutomatically: anObject [
	availableAutomatically := anObject
]

{ #category : 'accessing' }
TDebugger classSide >> closeAllDebuggers [
	^ self explicitRequirement
]

{ #category : 'instance creation' }
TDebugger classSide >> debugSession: aDebugSession [
	self explicitRequirement
]

{ #category : 'accessing' }
TDebugger classSide >> defaultDebuggerRank [

	^ DebuggerSettings defaultDebuggerRank
]

{ #category : 'debugging' }
TDebugger classSide >> handlesDebugSession: aDebugSession [

	^ SmalltalkImage current isInteractiveGraphic
]

{ #category : 'accessing' }
TDebugger classSide >> rank [
	^rank ifNil:[rank := self defaultDebuggerRank]
]

{ #category : 'accessing' }
TDebugger classSide >> rank: anInteger [
	rank := anInteger
]
