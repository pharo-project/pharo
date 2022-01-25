"
I am a command to install execution counter into given method or source node
"
Class {
	#name : #ClyAddExecutionCounterCommand,
	#superclass : #ClyDebuggingCommand,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-ExecutionCounters'
}

{ #category : #testing }
ClyAddExecutionCounterCommand class >> canBeExecutedInContext: aBrowserContext [

	^ (super canBeExecutedInContext: aBrowserContext) and: [ 
		  aBrowserContext isSelectedItemHasExecutionCounter not ]
]

{ #category : #activation }
ClyAddExecutionCounterCommand class >> contextMenuOrder [
	^2
]

{ #category : #activation }
ClyAddExecutionCounterCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: -0.5 for: ClySourceCodeContext
]

{ #category : #accessing }
ClyAddExecutionCounterCommand >> defaultMenuIconName [
	^#classIcon
]

{ #category : #accessing }
ClyAddExecutionCounterCommand >> defaultMenuItemName [
	^'Add Counter'
]

{ #category : #accessing }
ClyAddExecutionCounterCommand >> description [
	^ self defaultMenuItemName, ' to  ',  sourceNode displaySourceCode
]

{ #category : #execution }
ClyAddExecutionCounterCommand >> execute [
	
	ExecutionCounter installOn: sourceNode
]

{ #category : #execution }
ClyAddExecutionCounterCommand >> information [
	self defaultMenuItemName, ' to  ',  sourceNode displaySourceCode
]
