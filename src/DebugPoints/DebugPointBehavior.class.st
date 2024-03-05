"
I am the base class for different behaviors that can be added to all debug points.

In which order the behaviors are executed is decided by the priority variable.
The highest value is executed first.


API:
- #execute method should return true if execution should continue and false if it should interrupt.
- #remove method can be used if something has to be cleaned up when a behavior is removed from a debug point.
"
Class {
	#name : 'DebugPointBehavior',
	#superclass : 'Object',
	#instVars : [
		'priority',
		'debugPoint'
	],
	#category : 'DebugPoints-Base',
	#package : 'DebugPoints',
	#tag : 'Base'
}

{ #category : 'adding' }
DebugPointBehavior class >> addToDebugPoint: aDebugPoint [
	aDebugPoint addBehavior: (self new debugPoint: aDebugPoint).
	^aDebugPoint 
]

{ #category : 'accessing' }
DebugPointBehavior >> debugPoint [
	^ debugPoint 
]

{ #category : 'accessing' }
DebugPointBehavior >> debugPoint: aDebugPoint [
	debugPoint := aDebugPoint 
]

{ #category : 'execution' }
DebugPointBehavior >> execute [
	self subclassResponsibility
]

{ #category : 'accessing' }
DebugPointBehavior >> getArgument: aSymbol [
	^self debugPoint getArgument: aSymbol
]

{ #category : 'initialization' }
DebugPointBehavior >> initialize [ 
	self priority: 0
]

{ #category : 'accessing' }
DebugPointBehavior >> priority [
	^priority 
]

{ #category : 'accessing' }
DebugPointBehavior >> priority: anInteger [
	priority := anInteger 
]

{ #category : 'cleanup' }
DebugPointBehavior >> remove [
	"cleanup actions should be taken here"
	^self.
]
