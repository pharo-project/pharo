"
Description
--------------------

I am a morph that can be selected by a user in a FastTable.

I manage the color changes when the user is doing a mouse over or a selection.
"
Class {
	#name : #FTSelectableMorph,
	#superclass : #Morph,
	#instVars : [
		'selectionColor'
	],
	#category : #'Morphic-Widgets-FastTable'
}

{ #category : #initialization }
FTSelectableMorph >> defaultColor [
	^ Color transparent
]

{ #category : #initialization }
FTSelectableMorph >> initialize [
	super initialize.
	selectionColor := self defaultColor
]

{ #category : #'event handling' }
FTSelectableMorph >> mouseEnter: evt [
	self color: (selectionColor isTransparent ifTrue: [ self mouseOverColor ] ifFalse: [ self mouseOverColor mixed: 0.5 with: selectionColor ])
]

{ #category : #'event handling' }
FTSelectableMorph >> mouseLeave: evt [
	self color: self selectionColor
]

{ #category : #accessing }
FTSelectableMorph >> mouseOverColor [
	^ self theme lightBackgroundColor
]

{ #category : #initialization }
FTSelectableMorph >> selectionColor [
	^ selectionColor
]

{ #category : #accessing }
FTSelectableMorph >> selectionColor: aColor [
	selectionColor := aColor.
	self color: selectionColor
]

{ #category : #accessing }
FTSelectableMorph >> table [
	^ self owner ifNotNil: #table
]
