"
I am the base class for different behaviors that can be added to all debug points.

The priority slot value defines the order in which behaviors are executed.
The highest value is executed first.
However, all debug point check behaviors will be executed before all debug point side effect behaviors. Those are different types of debug points defined by subclasses.


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

{ #category : 'testing' }
DebugPointBehavior class >> isAbstract [

	^ true
]

{ #category : 'comparing' }
DebugPointBehavior >> <= aDebugPointBehavior [

	^ self priority <= aDebugPointBehavior priority
]

{ #category : 'adding' }
DebugPointBehavior >> addToDebugPoint: aDebugPoint [

	self subclassResponsibility 
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
	"Should return true if execution should continue and false if it should interrupt."

	self subclassResponsibility
]

{ #category : 'accessing' }
DebugPointBehavior >> getArgument: aSymbol [
	^self debugPoint getArgument: aSymbol
]

{ #category : 'initialization' }
DebugPointBehavior >> initialize [

	super initialize.
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
	"When a behavior is removed from a debug point, this method is invoked.
	By default do nothing."
	
	^ self
]

{ #category : 'removing' }
DebugPointBehavior >> removeFromDebugPoint: aDebugPoint [

	self subclassResponsibility
]
