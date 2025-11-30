"
General debugger settings with default values for TDebugger users
"
Class {
	#name : 'DebuggerSettings',
	#superclass : 'Object',
	#category : 'Debugger-Model-Core',
	#package : 'Debugger-Model',
	#tag : 'Core'
}

{ #category : 'accessing' }
DebuggerSettings class >> availableDebuggers [

	^ (self systemDebuggers select: [ :dbgClass |
		   dbgClass availableAutomatically ]) asOrderedCollection
]

{ #category : 'accessing' }
DebuggerSettings class >> defaultAvailability [
	^true
]

{ #category : 'accessing' }
DebuggerSettings class >> defaultDebuggerRank [
	^1
]

{ #category : 'accessing' }
DebuggerSettings class >> emergencyDebugger [

	^ self environment
		  at: #EDEmergencyDebugger
		  ifAbsent: [ DebuggerEmmergencyLogger new ]
]

{ #category : 'accessing' }
DebuggerSettings class >> systemDebuggers [

	^ (TDebugger users flatCollect: [ :class | class withAllSubclasses ])
		  reject: [ :class | class isTestCase ]
]
