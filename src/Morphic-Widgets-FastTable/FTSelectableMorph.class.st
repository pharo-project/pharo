Class {
	#name : #FTSelectableMorph,
	#superclass : #Morph,
	#instVars : [
		'selectionColor'
	],
	#category : #'Morphic-Widgets-FastTable'
}

{ #category : #initialize }
FTSelectableMorph >> defaultColor [
	^ Color transparent
]

{ #category : #'as yet unclassified' }
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

{ #category : #initialize }
FTSelectableMorph >> selectionColor [

	^selectionColor
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
