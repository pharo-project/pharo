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
	#category : #'Athens-Morphic'
}

{ #category : #accessing }
AthensParagraph >> caretRect [

	^ (0@0 corner: 0@0)
]

{ #category : #selection }
AthensParagraph >> characterBlockForIndex: index [

	^ self defaultCharacterBlock
]

{ #category : #composition }
AthensParagraph >> compose: aText style: aTextStyle from: startingIndex in: aRectangle [
	text := aText.
	textStyle := aTextStyle

	self shouldBeImplemented
]

{ #category : #private }
AthensParagraph >> compose: aText style: aTextStyle in: rect [

	text := aText.
	textStyle := aTextStyle.
	containerRect := rect.
	lines := (AthensTextComposer scan: aText for: AthensTextScanner new) lines
]

{ #category : #selection }
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

{ #category : #accessing }
AthensParagraph >> extent [
	^ containerRect width @ lines last bottom
]

{ #category : #selection }
AthensParagraph >> extraSelectionChanged [
	"
	refreshExtraSelection := true
"

	self flag: #pharoToRemove	"This method is only needed for TextMorphForEditView. This class is planned for removal with PluggableTextMorph. When it will be removed, this class can probably be cleaned."
]

{ #category : #selection }
AthensParagraph >> findReplaceSelectionRegex: aRegex [
	"	findReplaceSelectionRegex := aRegex. "

	self flag: #pharoToRemove	"This method is only needed for TextMorphForEditView. This class is planned for removal with PluggableTextMorph. When it will be removed, this class can probably be cleaned."
]

{ #category : #accessing }
AthensParagraph >> focused [
	^ false
]

{ #category : #accessing }
AthensParagraph >> focused: bool [

	"why we should care?"

	"self flag: #ooo"
]

{ #category : #selection }
AthensParagraph >> positionWhenComposed: pos [
	positionWhenComposed := pos
]

{ #category : #selection }
AthensParagraph >> secondarySelection: aSubstring [
	"	secondarySelection := aSubstring."

	self flag: #pharoToRemove	"This method is only needed for TextMorphForEditView. This class is planned for removal with PluggableTextMorph. When it will be removed, this class can probably be cleaned."
]

{ #category : #selection }
AthensParagraph >> selectionColor: anUndefinedObject [
	self shouldBeImplemented
]

{ #category : #selection }
AthensParagraph >> selectionRects [
	^ #()
]

{ #category : #selection }
AthensParagraph >> selectionStart: startBlock selectionStop: stopBlock [
	selectionStart := startBlock.
	selectionStop := stopBlock
]

{ #category : #accessing }
AthensParagraph >> showCaret [
]

{ #category : #accessing }
AthensParagraph >> showCaret: abool [
]

{ #category : #accessing }
AthensParagraph >> text [
	^ text
]

{ #category : #accessing }
AthensParagraph >> textOwner: anUndefinedObject [
	self shouldBeImplemented
]
