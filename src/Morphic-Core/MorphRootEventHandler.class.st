"
I handle meta gestures (special gesture, meta menu gesture, yellow button menu) for the morphs under a root morph, usually the world. A root keeps an ordered list of handlers like me; the morphs below it ask their root, and each handler is asked in turn until one answers that it handled the gesture.

I handle nothing: I answer false. With only Morphic's core loaded a root has no handlers, and meta gestures are ignored. Modules on top of the core, such as halos, add a subclass of me to the root: aWorld addRootEventHandler: aHandler.
"
Class {
	#name : 'MorphRootEventHandler',
	#superclass : 'Object',
	#category : 'Morphic-Core-Events',
	#package : 'Morphic-Core',
	#tag : 'Events'
}

{ #category : 'event handling' }
MorphRootEventHandler >> handleMetaMenuGesture: anEvent for: aMorph [
	"aMorph got the meta menu gesture. Answer whether I handled it: no"

	^ false
]

{ #category : 'event handling' }
MorphRootEventHandler >> handleSpecialGesture: anEvent for: aMorph [
	"aMorph got a special gesture (cmd-click on macOS, alt-click elsewhere). Answer whether I handled it: no"

	^ false
]

{ #category : 'event handling' }
MorphRootEventHandler >> handleYellowButtonMenu: shiftState for: aMorph [
	"aMorph wants its yellow button menu. Answer whether I opened one: no"

	^ false
]
