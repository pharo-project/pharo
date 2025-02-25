"
Description
--------------------

I am a morph that can be selected by a user in a FastTable.

I manage the color changes when the user is doing a mouse over or a selection.
"
Class {
	#name : 'FTSelectableMorph',
	#superclass : 'Morph',
	#instVars : [
		'selectionColor',
		'colorBeforeOver'
	],
	#category : 'Morphic-Widgets-FastTable-Base',
	#package : 'Morphic-Widgets-FastTable',
	#tag : 'Base'
}

{ #category : 'initialization' }
FTSelectableMorph >> defaultColor [
	^ Color transparent
]

{ #category : 'initialization' }
FTSelectableMorph >> initialize [
	super initialize.
	selectionColor := self defaultColor
]

{ #category : 'event handling' }
FTSelectableMorph >> mouseEnter: evt [

	colorBeforeOver := self color.
	self color: (selectionColor isTransparent
			 ifTrue: [ self mouseOverColor ]
			 ifFalse: [ self mouseOverColor mixed: 0.5 with: selectionColor ])
]

{ #category : 'event handling' }
FTSelectableMorph >> mouseLeave: evt [
	self color: colorBeforeOver
]

{ #category : 'accessing' }
FTSelectableMorph >> mouseOverColor [
	^ self theme mouseOverColor
]

{ #category : 'initialization' }
FTSelectableMorph >> selectionColor [
	^ selectionColor
]

{ #category : 'accessing' }
FTSelectableMorph >> selectionColor: aColor [
	selectionColor := aColor.
	self color: selectionColor
]

{ #category : 'accessing' }
FTSelectableMorph >> table [
	^ self owner ifNotNil: #table
]
