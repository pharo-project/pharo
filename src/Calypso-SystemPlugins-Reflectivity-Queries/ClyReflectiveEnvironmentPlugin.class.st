"
I am special plugin which decorate methods with various kind of metalinks: halts, breakpoint, counters and watchpoints.
And I provide related method groups
- breakpoints
- counters
- watchpoints
They group related methods together
"
Class {
	#name : #ClyReflectiveEnvironmentPlugin,
	#superclass : #ClySystemEnvironmentPlugin,
	#category : #'Calypso-SystemPlugins-Reflectivity-Queries-Plugin'
}

{ #category : #'method groups' }
ClyReflectiveEnvironmentPlugin >> collectMethodGroupProviders [
	^{ClyBreakpointMethodGroupProvider new. ClyMethodCounterGroupProvider new. ClyWatchpointMethodGroupProvider new}
]

{ #category : #'item decoration' }
ClyReflectiveEnvironmentPlugin >> decorateBrowserItem: anItem ofMethod: aMethod [

	aMethod containsHalt ifTrue: [ anItem markWith: ClyMethodWithHaltTag ].
	aMethod hasBreakpoint ifTrue: [ anItem markWith: ClyMethodWithBreakpointTag ].	

	(self doesMethodHasExecutionCounter: aMethod) 
		ifTrue: [ anItem markWith: ClyMethodWithExecutionCounterTag ].
	(self doesMethodHasWatchpoint: aMethod) 
		ifTrue: [ anItem markWith: ClyMethodWithWatchpointTag ]
]

{ #category : #'item decoration' }
ClyReflectiveEnvironmentPlugin >> doesAnyManager: metalinkManagers belongsTo: aMethod [

	^metalinkManagers anySatisfy: [ :each |
		each notNil and: [ self doesNodeOf: each belongsTo: aMethod]]
]

{ #category : #'item decoration' }
ClyReflectiveEnvironmentPlugin >> doesMethodHasExecutionCounter: aMethod [

	^self doesAnyManager: ExecutionCounter allCounters belongsTo: aMethod
]

{ #category : #'item decoration' }
ClyReflectiveEnvironmentPlugin >> doesMethodHasWatchpoint: aMethod [

	^self doesAnyManager: Watchpoint allWatchpoints belongsTo: aMethod
]

{ #category : #'item decoration' }
ClyReflectiveEnvironmentPlugin >> doesNodeOf: aMetalinkManager belongsTo: aMethod [

	| methodNode |
	methodNode := aMetalinkManager node methodNode.
	methodNode selector == aMethod selector ifFalse: [ ^false ].
	methodNode methodClass == aMethod methodClass ifFalse: [ ^false ].
	methodNode == aMethod ast ifFalse: [ ^false ].
	
	^true
]
