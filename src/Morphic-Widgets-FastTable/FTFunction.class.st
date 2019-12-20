"
I am an abstract class. My subclasses are some functions for a FastTable as search or filter.

Description
-------------------------------------------------
I can be use in two way.  Implicitely I will not appear on the FastTable. Explicitely I can display a Widget on the FastTable.
I work with  a FTTableMorph. I cannot be use alone.

Public API and Key Messages
-------------------------------------------------

- #table: aTableMorph
	is my constructor
	
- #keyStroke: anEvent
	This is the method that will allow to use me implicitely. With this I will receive an event from the FastTable.
	
- #beExplicite
	This method will make my functionnality explicit. For example the FTFilterFunction will display a filter field.
	
Example (Should only be create by a FTTableMorph)
-------------------------------------------------

FTFunction table: (FTTableMorph with: (1 to: 200))


Internal Representation and Key Implementation Points.
-------------------------------------------------

    Instance Variables
	table 	I am a FTTableMorph that use this function.

"
Class {
	#name : #FTFunction,
	#superclass : #Object,
	#instVars : [
		'table'
	],
	#category : #'Morphic-Widgets-FastTable-Functions'
}

{ #category : #testing }
FTFunction class >> isAbstract [
	^ self = FTFunction
]

{ #category : #'instance creation' }
FTFunction class >> new [
	self error: 'Use #table:'
]

{ #category : #'instance creation' }
FTFunction class >> table: aFastTableMorph [
	^ self basicNew
		initializeTable: aFastTableMorph;
		yourself
]

{ #category : #accessing }
FTFunction >> beExplicit [
	"This method is call by the FastTable if the user want the widget to be explicit. Describe what to do to be explicit."

	self subclassResponsibility
]

{ #category : #protocol }
FTFunction >> disable [
	"I am call when a FastTable disable a function."

	self subclassResponsibility
]

{ #category : #initialization }
FTFunction >> initializeTable: aTable [
	table := aTable.
	self initialize
]

{ #category : #testing }
FTFunction >> isExplicit [
	"See FTFunctionWithField to get an example."

	self subclassResponsibility
]

{ #category : #'event handling' }
FTFunction >> keyStroke: anEvent [
	"If the widget is implicit this method will be call if the user want to communicate with the widget."

	self subclassResponsibility
]

{ #category : #initialization }
FTFunction >> reset [
	self subclassResponsibility 
]

{ #category : #private }
FTFunction >> resizeContainerFrom: topLefPoint to: bottomRightPoint [
	"I takes takes the bounds of the container of the Fast Table. I return a rectangle that is the bounds of the container."

	self subclassResponsibility
]

{ #category : #private }
FTFunction >> resizeWidget [
	self subclassResponsibility 
]

{ #category : #accessing }
FTFunction >> showWidget [
	"A FastTable can have a special function. Most of these functions are hidden by default. This method make them explicit. 
	For exemple a FastTable can have a search field. By default the user need to type to show it. With this method the widget should be alwayse visible.
	My subclasses should describe what to do to be explicit."

	self subclassResponsibility
]

{ #category : #accessing }
FTFunction >> table [
	^ table
]
