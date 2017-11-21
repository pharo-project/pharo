"
I represent a default color for the terminal.
I can return the default color of the background and the default color of the font for this terminal.
"
Class {
	#name : #VTermDefaultColor,
	#superclass : #Object,
	#category : #System-CommandLine
}

{ #category : #terminfo }
VTermDefaultColor >> asTerminfoCharacterBackground [
	"I give a value to the background default color"
	^ (40 "Background indicator" + 9 "default color") asString
]

{ #category : #terminfo }
VTermDefaultColor >> asTerminfoCharacterFont [
	"I give a value to the font default color"
	^ (30 "Font indicator" + 9 "default color") asString
]

{ #category : #testing }
VTermDefaultColor >> isVTermDefaultColor [
	"A default color is a default color in anycase"
	
	^ true
]
