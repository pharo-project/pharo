"
Description
--------------------

I am a simple morph used by the MenubarMorph to add some separators. Use me via the #addSeparator method of MenubarMorph.
"
Class {
	#name : #MenubarSeparatorMorph,
	#superclass : #SimpleButtonMorph,
	#category : #'Morphic-Widgets-Menubar-View'
}

{ #category : #initialization }
MenubarSeparatorMorph >> defaultBorderWidth [
	^ 0
]

{ #category : #initialization }
MenubarSeparatorMorph >> defaultColor [
	^ Color transparent
]

{ #category : #initialization }
MenubarSeparatorMorph >> defaultLabel [
	^ '|'
]

{ #category : #initialization }
MenubarSeparatorMorph >> initialize [
	super initialize.
	self
		color: self defaultColor;
		borderWidth: self defaultBorderWidth
]

{ #category : #theme }
MenubarSeparatorMorph >> themeChanged [
	super themeChanged.
	self
		color: self defaultColor;
		borderWidth: self defaultBorderWidth
]
