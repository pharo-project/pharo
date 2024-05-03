"
I am the base class for different behaviors that can be added to all debug points and that perform side effects on the attached debug point or its context.




API:
- #execute method, returns self and performs side effects on the attached debug point or its context.
"
Class {
	#name : 'DebugPointMetaBehavior',
	#superclass : 'DebugPointBehavior',
	#category : 'DebugPoints-Base',
	#package : 'DebugPoints',
	#tag : 'Base'
}

{ #category : 'adding' }
DebugPointMetaBehavior >> addToDebugPoint: aDebugPoint [

	aDebugPoint addSideEffectBehavior: self
]

{ #category : 'removing' }
DebugPointMetaBehavior >> removeFromDebugPoint: aDebugPoint [

	aDebugPoint removeSideEffectBehavior: self
]
