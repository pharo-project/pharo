"
I represent the Debug API of methods/closures, providing offline access to the data.
For my main API, check `OnlineSourceDebugInfo`.

I'm an offline implementation: I contain the debug information previously computed.
In other words, I work even if the compiler is not available.

Internally, I contain a stop table representing the metadata at each interesting bytecode (suspension points) and a tree of (optimized) scopes.
For a better description of scopes, see `OnlineSourceDebugInfo` and the related scope classes.
"
Class {
	#name : 'OfflineSourceDebugInfo',
	#superclass : 'Object',
	#instVars : [
		'stopTable',
		'sourceCode',
		'rootScope',
		'stopTableDictionary'
	],
	#category : 'DebugInfo',
	#package : 'DebugInfo'
}

{ #category : 'accessing' }
OfflineSourceDebugInfo >> debugScopeAt: aBytecodePC [ 
	
	^ rootScope debugScopeAt: aBytecodePC
]

{ #category : 'private' }
OfflineSourceDebugInfo >> inflatedStopTableInfoFromFlags: flags [

	| stopTableInfo |
	stopTableInfo := OrderedCollection new.
	1 to: 3 "Num of flags" do: [ :e | 
		stopTableInfo addLast: (flags anyMask: 1) ].
	^ stopTableInfo
]

{ #category : 'api' }
OfflineSourceDebugInfo >> isArgumentNamed: aName at: aBytecodePC [

	^ (self debugScopeAt: aBytecodePC) isArgumentNamed: aName
]

{ #category : 'api' }
OfflineSourceDebugInfo >> isSourceReturn: aBytecodePC [

	^ (self stopTableDictionary at: aBytecodePC ifAbsent: [ ^ false ]) second
]

{ #category : 'api' }
OfflineSourceDebugInfo >> isSourceSend: aBytecodePC [ 
	
	^ (self stopTableDictionary at: aBytecodePC ifAbsent: [ ^ false ]) third
]

{ #category : 'api' }
OfflineSourceDebugInfo >> isSourceStore: aBytecodePC [ 
	
	^ (self stopTableDictionary at: aBytecodePC ifAbsent: [ ^ false ]) first
]

{ #category : 'private' }
OfflineSourceDebugInfo >> precedingBytecodePC: aBytecodePC [
	
	| found |
	self stopTableDictionary keys sorted do: [ :each |
		"If we went over the asked bytecode PC, we need to return the previous one"
		each >= aBytecodePC ifTrue: [ 
			"If asking for the bytecode before the first bytecode, fail"
			found ifNil: [ self error: 'There is no bytecode before PC ', aBytecodePC asString ].
			^ found ].
		found := each.
	].
]

{ #category : 'api' }
OfflineSourceDebugInfo >> rangeForPC: aBytecodePC [

	^ (self stopTableDictionary
		   at: aBytecodePC
		   ifAbsent: [ "If absent, somebody asked me for a *non-pc* because it is trying to guess.
		See for example Context >> #pcRangeContextIsActive:
		In that case, let's return the range of the preceding bytecodePC"
			   ^ self rangeForPC: (self precedingBytecodePC: aBytecodePC) ])
		  fourth
]

{ #category : 'api' }
OfflineSourceDebugInfo >> readVariableNamed: aName fromContext: aContext [

	^ (self debugScopeAt: aContext pc)
		  readVariableNamed: aName
		  fromContext: aContext
]

{ #category : 'accessing' }
OfflineSourceDebugInfo >> rootScope: aCachedDebugScope [ 
	rootScope := aCachedDebugScope
]

{ #category : 'accessing' }
OfflineSourceDebugInfo >> shouldStopAt: aBytecodePC [

	^ self stopTableDictionary includesKey: aBytecodePC
]

{ #category : 'accessing' }
OfflineSourceDebugInfo >> sourceCode [
	^ sourceCode
]

{ #category : 'accessing' }
OfflineSourceDebugInfo >> sourceCode: aString [ 
	sourceCode := aString
]

{ #category : 'accessing' }
OfflineSourceDebugInfo >> stopTable [
	
	^ stopTable
]

{ #category : 'accessing' }
OfflineSourceDebugInfo >> stopTable: aCollection [ 
	stopTable := aCollection
]

{ #category : 'private' }
OfflineSourceDebugInfo >> stopTableDictionary [

	stopTableDictionary ifNotNil: [ ^ stopTableDictionary ].


	stopTableDictionary := (stopTable groupsOf: 4 atATimeCollect: [ :e |
			                        | stopTableFlags stopTableRange stopTableInflatedInfo |
			                        stopTableFlags := e second.
			                        stopTableRange := e third to: e fourth.
			                        stopTableInflatedInfo := (self
				                                                  inflatedStopTableInfoFromFlags:
				                                                  stopTableFlags)
				                                                 copyWith:
				                                                 stopTableRange.

			                        e first -> stopTableInflatedInfo ])
		                       asDictionary.
	^ stopTableDictionary
]

{ #category : 'accessing' }
OfflineSourceDebugInfo >> variableNamesAt: aBytecodePC [

	^ (self debugScopeAt: aBytecodePC) variableNames
]

{ #category : 'accessing' }
OfflineSourceDebugInfo >> variablesAt: aBytecodePC [

	^ (self debugScopeAt: aBytecodePC) variables
]
