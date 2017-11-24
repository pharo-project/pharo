Class {
	#name : #RubExtraSelectionDecorator,
	#superclass : #RubParagraphDecorator,
	#instVars : [
		'findReplaceRanges',
		'findReplaceShapes',
		'secondarySelectionShapes'
	],
	#category : #'Rubric-Editing-Core'
}

{ #category : #querying }
RubExtraSelectionDecorator class >> key [
	^ #findReplaceSelection
]

{ #category : #computation }
RubExtraSelectionDecorator >> cleanUpText [
	self text removeAttribute: RubTextSelectionColor findReplaceSelection.
	self text removeAttribute: RubTextSelectionColor secondarySelection.

]

{ #category : #computation }
RubExtraSelectionDecorator >> computeFindReplaceShapesFrom: topLeft to: bottomRight [
	| topLeftBlk |
	findReplaceShapes := nil.
	self findRegex ifNil: [ ^self ].
	topLeftBlk := self characterBlockAtPoint: topLeft.
	findReplaceRanges := ((self findRegex matchingRangesIn: (self visibleStringFrom: topLeft to: bottomRight))
		collect: [ :r | r + topLeftBlk stringIndex - 1 ]).
	findReplaceRanges do: [ :r | self text addAttribute: RubTextSelectionColor findReplaceSelection from: r first to: r last ].
	findReplaceShapes := findReplaceRanges
		collect: [ :r | 
			RubSurfaceSelectionShape
				vertices: (self verticesFrom: r first to: r last + 1)
				firstIndex: r first
				lastIndex: r last + 1
				color: (RubTextSelectionColor findReplaceSelection backgroundColor alpha: 0.1)
				borderWidth: 2
				borderColor: RubTextSelectionColor findReplaceSelection backgroundColor muchDarker].
	^ findReplaceShapes
]

{ #category : #drawing }
RubExtraSelectionDecorator >> computeSecondarySelectionShapesFrom: topLeft to: bottomRight [
	| primary secondarySelRanges otherRanges topLeftBlk |
	primary := self primarySelectionInterval.
	topLeftBlk := self characterBlockAtPoint: topLeft.
	otherRanges := findReplaceRanges ifNil: [ #() ].
	self selection asString trimBoth ifEmpty: [ ^secondarySelectionShapes := #() ].
	(secondarySelRanges := (((self visibleStringFrom: topLeft to: bottomRight) allRangesOfSubstring: self selection asString)
		collect: [ :r | r + topLeftBlk stringIndex - 1 ])
		reject: [ :i | 
			(otherRanges anySatisfy: [ :r | (r rangeIncludes: i first) or: [ r rangeIncludes: i last ] ])
				or: [ i = primary or: [ i first > self text size ] ] ]) collect: [ :i | i first to: (i last min: self text size) ].
	secondarySelRanges
		do: [ :r | self text addAttribute: RubTextSelectionColor secondarySelection from: r first to: r last ].
	^ secondarySelectionShapes := secondarySelRanges
		collect: [ :r | 
			RubSurfaceSelectionShape
				vertices: (self verticesFrom: r first to: r last + 1)
				firstIndex: r first
				lastIndex: r last + 1
				color: (RubTextSelectionColor secondarySelection backgroundColor)
				borderWidth: 0
				borderColor: RubTextSelectionColor secondarySelection backgroundColor muchDarker]
]

{ #category : #drawing }
RubExtraSelectionDecorator >> computeSelectionShapesFor: aRectangle [
	self computeFindReplaceShapesFrom: aRectangle topLeft to: aRectangle bottomRight.
	self computeSecondarySelectionShapesFrom: aRectangle topLeft to: aRectangle bottomRight
]

{ #category : #drawing }
RubExtraSelectionDecorator >> drawAllSelectionShapesOn: aCanvas [

	findReplaceShapes ifNotNil: [ 
		findReplaceShapes do: [ :selblock | 
			self drawSelectionShape: selblock on: aCanvas
		] 
	].

	secondarySelectionShapes ifNotNil: [ 
		secondarySelectionShapes do: [ :selblock | 
				self drawSelectionShape: selblock on: aCanvas 
		] 
	]
]

{ #category : #drawing }
RubExtraSelectionDecorator >> drawAllSelectionShapesOnAthensCanvas: anAthensCanvas [
	findReplaceShapes ifNotNil: [ findReplaceShapes do: [ :selblock | self drawSelectionShape: selblock onAthensCanvas: anAthensCanvas ] ].
	secondarySelectionShapes ifNotNil: [ secondarySelectionShapes do: [ :selblock | self drawSelectionShape:  selblock onAthensCanvas: anAthensCanvas] ]
]

{ #category : #drawing }
RubExtraSelectionDecorator >> drawOn: aCanvas [

	(Display depth < 3 or: [ aCanvas isShadowDrawing ]) ifTrue: [ ^ self ].		
	
	self computeSelectionShapesFor: aCanvas clipRect.
	self cleanUpText.
	self drawAllSelectionShapesOn: aCanvas
]

{ #category : #drawing }
RubExtraSelectionDecorator >> drawOnAthensCanvas: anAthensCanvas [
	super drawOnAthensCanvas: anAthensCanvas.
	self computeSelectionShapesFor: anAthensCanvas clipRect.
	self cleanUpText.
	self drawAllSelectionShapesOnAthensCanvas: anAthensCanvas
]

{ #category : #drawing }
RubExtraSelectionDecorator >> drawSelectionShape: aSelBlock on: aCanvas [
	"Display a SelectionShape if it does not overlap vith the regular selection"
	
	| startIdx stopIdx  |
	
	startIdx := aSelBlock firstIndex.
	stopIdx := aSelBlock lastIndex.
	(self hasValidSelection) ifTrue: [ 
			| selSartIdx selStopIdx selBlockRange selRange |
			selSartIdx := self paragraph selectionStart stringIndex.
			selStopIdx := self paragraph selectionStop stringIndex.
			selBlockRange := startIdx to: stopIdx.
			selRange := selSartIdx to: selStopIdx.
			((selBlockRange rangeIncludes: selSartIdx + 1) or: [ 
					(selBlockRange rangeIncludes: selStopIdx - 1) 	or: [ 
							(selRange rangeIncludes: startIdx + 1) or: [ 
								selRange rangeIncludes: stopIdx - 1 ] 
							] 
					]
			)ifTrue: [ ^ self ] 
		].	"regular selection and this selection block overlap"
	aSelBlock drawOn: aCanvas 
]

{ #category : #drawing }
RubExtraSelectionDecorator >> drawSelectionShape: aSelBlock onAthensCanvas: anAthensCanvas [
	"Display a SelectionShape if it does not overlap vith the regular selection"
	| startIdx stopIdx  |
	startIdx := aSelBlock firstIndex.
	stopIdx := aSelBlock lastIndex.
	self hasValidSelection ifTrue: [ 
		| selSartIdx selStopIdx selBlockRange selRange |
		
		selSartIdx := self selectionStart stringIndex.
		selStopIdx := self selectionStop stringIndex.
		selBlockRange := startIdx to: stopIdx.
		selRange := selSartIdx to: selStopIdx.
		((selBlockRange rangeIncludes: selSartIdx + 1)
				or: [ 
					(selBlockRange rangeIncludes: selStopIdx - 1)
						or: [ (selRange rangeIncludes: startIdx + 1) or: [ selRange rangeIncludes: stopIdx - 1 ] ] ])
				ifTrue: [ ^ self ] ].	"regular selection and this selection block overlap"
	aSelBlock drawOnAthensCanvas: anAthensCanvas
]

{ #category : #accessing }
RubExtraSelectionDecorator >> findRegex [
	^ self paragraph textArea ifNotNil: [ :textArea | textArea findRegex ]
]

{ #category : #testing }
RubExtraSelectionDecorator >> hasValidSelection [

	| paragraph |
	
	paragraph := self paragraph ifNil: [ ^false ].
	
	^paragraph selectionStart notNil and: [ 
		paragraph selectionStop notNil and: [ 
			paragraph selectionStart ~= paragraph selectionStop 
		] 
	]
]

{ #category : #accessing }
RubExtraSelectionDecorator >> primarySelectionInterval [
	
	self paragraph ifNotNil: [ :paragraph | 
		paragraph textArea ifNotNil: [ :textArea | 
			(textArea selectionStart isNotNil and: [ 
				textArea selectionStop isNotNil  		
			]) ifTrue: [ 
				^textArea selectionStart stringIndex to: textArea selectionStop stringIndex - 1
			]
		]	
	].

   ^0 to: -1

]
