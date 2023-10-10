"
I am an abstract class. My subclasses are some functions for a FastTable that need a field when they are use explicitely.

Description
-------------------------------------------------
I act as my superclass but I manage a field. When the user type in the field an Announcement will call #update:

Public API and Key Messages
-------------------------------------------------

- #update: anAnnoucement
	is call when the user type something into the field
	
Example (Should only be create by a FTTableMorph)
-------------------------------------------------

FTFunctionWithField table: (FTTableMorph with: (1 to: 200))

Internal Representation and Key Implementation Points.
-------------------------------------------------

    Instance Variables
	field:		I am the field to display at the bottom of the FTTableMorph
"
Class {
	#name : 'FTFieldFunction',
	#superclass : 'FTFunction',
	#instVars : [
		'field'
	],
	#category : 'Morphic-Widgets-FastTable-Functions',
	#package : 'Morphic-Widgets-FastTable',
	#tag : 'Functions'
}

{ #category : 'testing' }
FTFieldFunction class >> isAbstract [
	^ self = FTFieldFunction
]

{ #category : 'accessing' }
FTFieldFunction >> beExplicit [
	self initializeMorph.
	table
		addMorph: field;
		resizeAllSubviews
]

{ #category : 'protocol' }
FTFieldFunction >> disable [
	field
		ifNotNil: [ field delete.
			field := nil ]
]

{ #category : 'accessing' }
FTFieldFunction >> fieldHeigh [
	^ StandardFonts defaultFont pixelSize + 10
]

{ #category : 'widget API' }
FTFieldFunction >> ghostText [
	self subclassResponsibility
]

{ #category : 'initialization' }
FTFieldFunction >> initializeMorph [
	field := RubScrolledTextModel new newTextField
		hResizing: #spaceFill;
		autoAccept: true;
		withGhostText: self ghostText;
		yourself.
	field model announcer when: RubTextUpdatedInModel send: #textUpdated: to: self
]

{ #category : 'testing' }
FTFieldFunction >> isExplicit [
	^ field isNotNil
]

{ #category : 'private' }
FTFieldFunction >> resizeContainerFrom: topLeftPoint to: bottomLeftPoint [
	^ topLeftPoint corner: bottomLeftPoint x @ (bottomLeftPoint y - self fieldHeigh)
]

{ #category : 'private' }
FTFieldFunction >> resizeWidget [
	| topLeft bottomRight |
	self flag: #pharoTodo.	"Maybe it could be cool to let the user choose if the field need to be at the top or the bottom."
	topLeft := table bounds left @ (table bounds bottom - self fieldHeigh - table horizontalScrollBarHeight ).
	bottomRight := (table bounds right - table verticalScrollBarWidth) @ (table bottom - table horizontalScrollBarHeight).
	field bounds: (topLeft corner: bottomRight)
]

{ #category : 'accessing' }
FTFieldFunction >> showWidget [
	self initializeMorph.
	table
		addMorph: field;
		resizeAllSubviews
]

{ #category : 'updating' }
FTFieldFunction >> textUpdated: anAnnouncement [

	"do nothing by default"
]
