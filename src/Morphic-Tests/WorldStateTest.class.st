"
I check how a `WorldState` reacts when drawing its world fails.

I stand for the world that fails to draw and for the error handler, so the tests touch no global world or handler.
"
Class {
	#name : 'WorldStateTest',
	#superclass : 'TestCase',
	#instVars : [
		'handledErrors'
	],
	#category : 'Morphic-Tests-Kernel',
	#package : 'Morphic-Tests',
	#tag : 'Kernel'
}

{ #category : 'helpers' }
WorldStateTest >> displayWorld [
	"I stand for a world whose drawing fails outside of any morph"

	self error: 'drawing failed'
]

{ #category : 'helpers' }
WorldStateTest >> handleError: anError log: shouldLog [
	"I stand for the error handler of the world state"

	handledErrors add: anError
]

{ #category : 'tests' }
WorldStateTest >> testFatalDrawingErrorGoesToTheErrorHandler [

	| state |
	handledErrors := OrderedCollection new.
	state := WorldState new.
	state worldRenderer: (NullWorldRenderer forWorld: WorldMorph new).
	state errorHandler: self.

	state displayWorldSafely: self.

	self assert: handledErrors size equals: 1.
	self assert: handledErrors first messageText equals: 'drawing failed'
]
