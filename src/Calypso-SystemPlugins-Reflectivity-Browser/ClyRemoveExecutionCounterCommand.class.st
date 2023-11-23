"
I am a command to remove execution counters from given method of node
"
Class {
	#name : 'ClyRemoveExecutionCounterCommand',
	#superclass : 'ClyRemoveDebuggingCommand',
	#category : 'Calypso-SystemPlugins-Reflectivity-Browser-ExecutionCounters',
	#package : 'Calypso-SystemPlugins-Reflectivity-Browser',
	#tag : 'ExecutionCounters'
}

{ #category : 'testing' }
ClyRemoveExecutionCounterCommand class >> canBeExecutedInContext: aBrowserContext [

	^aBrowserContext isSelectedItemHasExecutionCounter
]

{ #category : 'activation' }
ClyRemoveExecutionCounterCommand class >> contextMenuOrder [
	^2
]

{ #category : 'activation' }
ClyRemoveExecutionCounterCommand class >> methodContextMenuActivation [

	<classAnnotation>
	^ SycAdvancedDebuggingMenuActivation
			byItemOf: ClyAdvancedDebuggingMenuGroup order: self contextMenuOrder for: ClyMethod asCalypsoItemContext
]

{ #category : 'activation' }
ClyRemoveExecutionCounterCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycAdvancedDebuggingMenuActivation
		byItemOf: ClyAdvancedDebuggingMenuGroup order: self contextMenuOrder for: ClyMethodSourceCodeContext
]

{ #category : 'accessing' }
ClyRemoveExecutionCounterCommand >> createTableCellButtonWith: anIcon using: aCommandActivator [
	| button |
	button := super createTableCellButtonWith: anIcon using: aCommandActivator.
	sourceNode hasExecutionCounter ifTrue: [
		ClyDynamicBallonTooltip from: [self tableDescription] installInto: button].
	^button
]

{ #category : 'accessing' }
ClyRemoveExecutionCounterCommand >> currentCallsCount [

	^sourceNode counter count
]

{ #category : 'execution' }
ClyRemoveExecutionCounterCommand >> debuggingToolClass [
	^ExecutionCounter
]

{ #category : 'accessing' }
ClyRemoveExecutionCounterCommand >> defaultMenuItemName [

	^sourceNode hasExecutionCounter
		ifTrue: [ 'Remove counter (', self currentCallsCount asString, ')']
	 	ifFalse: [ 'Remove counter' ]
]

{ #category : 'table decoration' }
ClyRemoveExecutionCounterCommand >> tableCellIcon [
	^Smalltalk ui iconNamed: #class
]

{ #category : 'accessing' }
ClyRemoveExecutionCounterCommand >> tableDescription [
	^'Sent ', self currentCallsCount asString, ' times. Click to remove'
]
