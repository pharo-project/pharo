"
I am a shortcut handler that installs keymappings in the system.
"
Class {
	#name : 'KMShortcutHandler',
	#superclass : 'Object',
	#category : 'Keymapping-Core-Base',
	#package : 'Keymapping-Core',
	#tag : 'Base'
}

{ #category : 'shortcut-handling' }
KMShortcutHandler >> handleKeystroke: aKeystrokeEvent inMorph: aMorph [

	aMorph handleKeystrokeWithKeymappings: aKeystrokeEvent
]
