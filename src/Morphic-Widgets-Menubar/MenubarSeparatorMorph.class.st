Class {
	#name : #MenubarSeparatorMorph,
	#superclass : #SimpleButtonMorph,
	#category : #'Morphic-Widgets-Menubar'
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
