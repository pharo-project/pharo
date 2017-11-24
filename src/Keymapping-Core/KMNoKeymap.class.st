"
I am a Null object representing the absence of a Keymap.
"
Class {
	#name : #KMNoKeymap,
	#superclass : #Object,
	#category : #'Keymapping-Core'
}

{ #category : #accessing }
KMNoKeymap >> shortcut [
	^ KMNoShortcut new
]
