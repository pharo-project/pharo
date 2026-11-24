"
I take events at a world before it dispatches them. A world keeps an ordered list of handlers like me and offers each event to them in turn (handleEvent:in:), before sending it to its hand's focus holder or to its morphs. The first handler that answers true takes the event.

I take nothing: I answer false. With only Morphic's core loaded a world has no handlers. Modules on top of the core, such as halos, add a subclass of me to the world: aWorld addRootEventHandler: aHandler.
"
Class {
	#name : 'MorphRootEventHandler',
	#superclass : 'Object',
	#category : 'Morphic-Core-Events',
	#package : 'Morphic-Core',
	#tag : 'Events'
}

{ #category : 'event handling' }
MorphRootEventHandler >> handleEvent: anEvent in: aWorld [
	"aWorld got anEvent and is about to dispatch it. Answer whether I took it instead: no"

	^ false
]

