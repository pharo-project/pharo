"
I am a null shortcut handler for morphic. I register myself as a tool to be replaced by somebody else who really wants to handle shortcuts.
"
Class {
	#name : 'MorphicShortcutHandler',
	#superclass : 'Object',
	#category : 'Morphic-Core-Events',
	#package : 'Morphic-Core',
	#tag : 'Events'
}

{ #category : 'shortcut-handling' }
MorphicShortcutHandler >> handleKeystroke: aKeystrokeEvent inMorph: aMorph [

	"I do nothing. I'm a null object"
]
