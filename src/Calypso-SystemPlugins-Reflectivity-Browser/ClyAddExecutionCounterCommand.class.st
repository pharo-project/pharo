"
I am a command to install execution counter into given method or source node
"
Class {
	#name : 'ClyAddExecutionCounterCommand',
	#superclass : 'ClyAdvancedDebuggingCommand',
	#category : 'Calypso-SystemPlugins-Reflectivity-Browser-ExecutionCounters',
	#package : 'Calypso-SystemPlugins-Reflectivity-Browser',
	#tag : 'ExecutionCounters'
}

{ #category : 'testing' }
ClyAddExecutionCounterCommand class >> canBeExecutedInContext: aBrowserContext [

	^ (super canBeExecutedInContext: aBrowserContext) and: [
		  aBrowserContext isSelectedItemHasExecutionCounter not ]
]

{ #category : 'activation' }
ClyAddExecutionCounterCommand class >> contextMenuOrder [
	^2
]

{ #category : 'accessing' }
ClyAddExecutionCounterCommand >> defaultMenuIconName [
	^#class
]

{ #category : 'accessing' }
ClyAddExecutionCounterCommand >> defaultMenuItemName [
	^' Add     Counter    to  ', sourceNode displaySourceCode
]

{ #category : 'accessing' }
ClyAddExecutionCounterCommand >> description [
	^ self defaultMenuItemName, ' to  ',  sourceNode displaySourceCode
]

{ #category : 'execution' }
ClyAddExecutionCounterCommand >> execute [

	ExecutionCounter installOn: sourceNode
]

{ #category : 'execution' }
ClyAddExecutionCounterCommand >> information [
	self defaultMenuItemName, ' to  ',  sourceNode displaySourceCode
]
