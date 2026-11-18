"
I handle the meta gestures (special gesture, meta menu gesture) of the morphs under a root morph, usually the world. A root keeps one of me; the morphs below it ask their root for me.

I do nothing: with only Morphic's core loaded, meta gestures are ignored. Modules on top of the core, such as halos, set a subclass of me on the root to give these gestures a meaning.
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
	"aMorph got the meta menu gesture. Ignore it"

	
]

{ #category : 'event handling' }
MorphRootEventHandler >> handleSpecialGesture: anEvent for: aMorph [
	"aMorph got a special gesture (cmd-click on macOS, alt-click elsewhere). Ignore it"

	
]
