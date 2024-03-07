"
I am the base class for different behaviors that can be added to all debug points and that determine whether the debug point should be hit or not.




API:
- #execute method should return true if execution should continue and false if it should interrupt.
"
Class {
	#name : 'DebugPointCheckBehavior',
	#superclass : 'DebugPointBehavior',
	#category : 'DebugPoints-Base',
	#package : 'DebugPoints',
	#tag : 'Base'
}

{ #category : 'adding' }
DebugPointCheckBehavior >> addToDebugPoint: aDebugPoint [

	aDebugPoint addCheckBehavior: self
]

{ #category : 'initialization' }
DebugPointCheckBehavior >> initialize [

	super initialize.
	self priority: SmallInteger maxVal
]
