"
this class is subject of changes/removal. do not use it.
"
Class {
	#name : #AthensParagraph,
	#superclass : #Object,
	#instVars : [
		'text',
		'textStyle',
		'containerRect',
		'lines',
		'positionWhenComposed',
		'selectionStart',
		'selectionStop'
	],
	#category : #Athens-Morphic
}

{ #category : #'as yet unclassified' }
AthensParagraph >> caretRect [ ^ (0@0 corner: 0@0)  
]

{ #category : #'as yet unclassified' }
AthensParagraph >> characterBlockForIndex: index [ 

		^ self defaultCharacterBlock
]

{ #category : #'as yet unclassified' }
AthensParagraph >> compose: aText style: aTextStyle from: startingIndex in: aRectangle [ 
	text := aText.
	textStyle := aTextStyle
	
	self shouldBeImplemented.
]

{ #category : #'as yet unclassified' }
AthensParagraph >> compose: aText style: aTextStyle in: rect [ 

	text := aText.
	textStyle := aTextStyle.
	containerRect := rect.
	lines := (AthensTextComposer scan: aText for: AthensTextScanner new) lines.
]

{ #category : #'as yet unclassified' }
AthensParagraph >> defaultCharacterBlock [
	^ (CharacterBlock new stringIndex: 1 text: text
			topLeft: 0@0 extent: 0 @ 0)
		textLine: lines first
]

{ #category : #drawing }
AthensParagraph >> drawOnAthensCanvas: canvas bounds: aRectangle [ 

	| renderer |
	"self halt."
"	canvas
		clipBy: aRectangle 
		during: [  
"			renderer := AthensTextRenderer new.
			renderer render: text lines: lines on: canvas.
"		].
"
]

{ #category : #'as yet unclassified' }
AthensParagraph >> extent [
	^ containerRect width @ lines last bottom
]

{ #category : #'as yet unclassified' }
AthensParagraph >> extraSelectionChanged [
"
	refreshExtraSelection := true
"

	self flag: #foo
]

{ #category : #'as yet unclassified' }
AthensParagraph >> findReplaceSelectionRegex: aRegex [
"	findReplaceSelectionRegex := aRegex. "
	self flag: #foo

]

{ #category : #'as yet unclassified' }
AthensParagraph >> focused [ ^ false
]

{ #category : #'as yet unclassified' }
AthensParagraph >> focused: bool [

	"why we should care?"
	
	"self flag: #ooo"
]

{ #category : #'as yet unclassified' }
AthensParagraph >> positionWhenComposed: pos [
	positionWhenComposed := pos
]

{ #category : #'as yet unclassified' }
AthensParagraph >> secondarySelection: aSubstring [
"	secondarySelection := aSubstring."
	self flag: #foo.
]

{ #category : #'as yet unclassified' }
AthensParagraph >> selectionColor: anUndefinedObject [ 
	self shouldBeImplemented.
]

{ #category : #'as yet unclassified' }
AthensParagraph >> selectionRects [
	^ #()
]

{ #category : #'as yet unclassified' }
AthensParagraph >> selectionStart: startBlock selectionStop: stopBlock [
	selectionStart := startBlock.
	selectionStop := stopBlock.
]

{ #category : #'as yet unclassified' }
AthensParagraph >> showCared [ 
]

{ #category : #'as yet unclassified' }
AthensParagraph >> showCaret [  
]

{ #category : #'as yet unclassified' }
AthensParagraph >> showCaret: abool [  
]

{ #category : #'as yet unclassified' }
AthensParagraph >> text [
	^ text
]

{ #category : #'as yet unclassified' }
AthensParagraph >> textOwner: anUndefinedObject [ 
	self shouldBeImplemented.
]
