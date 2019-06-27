"
I am a command to remove execution counters from given method of node
"
Class {
	#name : #ClyRemoveExecutionCounterCommand,
	#superclass : #ClyRemoveMetalinkCommand,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-ExecutionCounters'
}

{ #category : #execution }
ClyRemoveExecutionCounterCommand class >> canBeExecutedInContext: aBrowserContext [

	^aBrowserContext isSelectedItemHasExecutionCounter 
]

{ #category : #activation }
ClyRemoveExecutionCounterCommand class >> contextMenuOrder [	
	^2
]

{ #category : #accessing }
ClyRemoveExecutionCounterCommand >> createTableCellButtonWith: anIcon using: aCommandActivator [
	| button |
	button := super createTableCellButtonWith: anIcon using: aCommandActivator.
	sourceNode hasExecutionCounter ifTrue: [ 
		ClyDynamicBallonTooltip from: [self tableDescription] installInto: button].
	^button
]

{ #category : #accessing }
ClyRemoveExecutionCounterCommand >> currentCallsCount [
	
	^sourceNode counter count
]

{ #category : #accessing }
ClyRemoveExecutionCounterCommand >> defaultMenuItemName [
	
	^sourceNode hasExecutionCounter 
		ifTrue: [ 'Remove counter (', self currentCallsCount asString, ')']
	 	ifFalse: [ 'Remove counter' ]
]

{ #category : #execution }
ClyRemoveExecutionCounterCommand >> metalinkManagerClass [
	^ExecutionCounter
]

{ #category : #'table decoration' }
ClyRemoveExecutionCounterCommand >> tableCellIcon [
	^Smalltalk ui iconNamed: #classIcon
]

{ #category : #accessing }
ClyRemoveExecutionCounterCommand >> tableDescription [
	^'Sent ', self currentCallsCount asString, ' times. Click to remove'
]
