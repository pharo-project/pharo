Class {
	#name : 'RubExtraSelectionDecorator',
	#superclass : 'RubParagraphDecorator',
	#instVars : [
		'findReplaceRanges',
		'findReplaceShapes',
		'secondarySelectionShapes',
		'currentMatchShape'
	],
	#category : 'Morphic-Widgets-Editing-Core',
	#package : 'Morphic-Widgets',
	#tag : 'Editing-Core'
}

{ #category : 'querying' }
RubExtraSelectionDecorator class >> key [
	^ #findReplaceSelection
]

{ #category : 'computation' }
RubExtraSelectionDecorator >> cleanUpText [
	self text removeAttribute: RubTextSelectionColor secondarySelection
]

{ #category : 'computation' }
RubExtraSelectionDecorator >> computeFindReplaceShapesFrom: topLeft to: bottomRight [
	"Compute the shapes of the visible find matches from the ranges cached by the find and replace service: nothing is searched while drawing, and nothing at all when no search is active.
	The other matches get a soft fill. The current match (the selection, when it is a match) gets a stronger fill and a border, so that it stays visible while the keyboard focus is in the find bar."

	| service startIndex stopIndex current matchColor |
	findReplaceShapes := nil.
	currentMatchShape := nil.
	findReplaceRanges := nil.
	service := self findReplaceService ifNil: [ ^ self ].
	service hasActiveSearch ifFalse: [ ^ self ].
	startIndex := (self characterBlockAtPoint: topLeft) stringIndex.
	stopIndex := (self characterBlockAtPoint: bottomRight) stringIndex.
	findReplaceRanges := service matchRangesFrom: startIndex to: stopIndex.
	findReplaceRanges ifEmpty: [ ^ self ].
	current := self primarySelectionInterval.
	(findReplaceRanges includes: current) ifFalse: [ current := nil ].
	matchColor := self findMatchColor.
	findReplaceShapes := OrderedCollection new.
	findReplaceRanges do: [ :r |
		| shape |
		shape := RubSurfaceSelectionShape
			         vertices: (self verticesFrom: r first to: r last + 1)
			         firstIndex: r first
			         lastIndex: r last + 1
			         color: (r = current
					          ifTrue: [ matchColor ]
					          ifFalse: [ matchColor alpha: matchColor alpha * 0.45 ])
			         borderWidth: (r = current ifTrue: [ 1 ] ifFalse: [ 0 ])
			         borderColor: (r = current
					          ifTrue: [ matchColor alpha: 1 ]
					          ifFalse: [ Color transparent ]).
		r = current
			ifTrue: [ currentMatchShape := shape ]
			ifFalse: [ findReplaceShapes add: shape ] ].
	^ findReplaceShapes
]

{ #category : 'drawing' }
RubExtraSelectionDecorator >> computeSecondarySelectionShapesFrom: topLeft to: bottomRight [

	| primary selection secondarySelRanges otherRanges topLeftBlk |
	primary := self primarySelectionInterval.
	topLeftBlk := self characterBlockAtPoint: topLeft.
	otherRanges := findReplaceRanges ifNil: [ #(  ) ].
	selection := self selection asString.
	selection trimBoth ifEmpty: [ ^ secondarySelectionShapes := #(  ) ].
	(secondarySelRanges := (((self visibleStringFrom: topLeft to: bottomRight) allRangesOfSubstring: selection) collect: [ :r | r + topLeftBlk stringIndex - 1 ])
		                       reject: [ :i |
		                       (otherRanges anySatisfy: [ :r | (r rangeIncludes: i first) or: [ r rangeIncludes: i last ] ]) or: [
			                       i = primary or: [ i first > self text size ] ] ]) collect: [ :i | i first to: (i last min: self text size) ].
	secondarySelRanges do: [ :r | self text addAttribute: RubTextSelectionColor secondarySelection from: r first to: r last ].
	^ secondarySelectionShapes := secondarySelRanges collect: [ :r |
			                              RubSurfaceSelectionShape
				                              vertices: (self verticesFrom: r first to: r last + 1)
				                              firstIndex: r first
				                              lastIndex: r last + 1
				                              color: RubTextSelectionColor secondarySelection backgroundColor
				                              borderWidth: 0
				                              borderColor: RubTextSelectionColor secondarySelection backgroundColor muchDarker ]
]

{ #category : 'drawing' }
RubExtraSelectionDecorator >> computeSelectionShapesFor: aRectangle [
	self computeFindReplaceShapesFrom: aRectangle topLeft to: aRectangle bottomRight.
	self computeSecondarySelectionShapesFrom: aRectangle topLeft to: aRectangle bottomRight
]

{ #category : 'drawing' }
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
	].
	currentMatchShape ifNotNil: [ :shape | shape drawOn: aCanvas ]
]

{ #category : 'drawing' }
RubExtraSelectionDecorator >> drawAllSelectionShapesOnAthensCanvas: anAthensCanvas [
	findReplaceShapes ifNotNil: [ findReplaceShapes do: [ :selblock | self drawSelectionShape: selblock onAthensCanvas: anAthensCanvas ] ].
	secondarySelectionShapes ifNotNil: [ secondarySelectionShapes do: [ :selblock | self drawSelectionShape:  selblock onAthensCanvas: anAthensCanvas] ].
	currentMatchShape ifNotNil: [ :shape | shape drawOnAthensCanvas: anAthensCanvas ]
]

{ #category : 'drawing' }
RubExtraSelectionDecorator >> drawOn: aCanvas [

	aCanvas isShadowDrawing ifTrue: [ ^ self ].

	self computeSelectionShapesFor: aCanvas clipRect.
	self cleanUpText.
	self drawAllSelectionShapesOn: aCanvas
]

{ #category : 'drawing' }
RubExtraSelectionDecorator >> drawOnAthensCanvas: anAthensCanvas [
	super drawOnAthensCanvas: anAthensCanvas.
	self computeSelectionShapesFor: anAthensCanvas clipRect.
	self cleanUpText.
	self drawAllSelectionShapesOnAthensCanvas: anAthensCanvas
]

{ #category : 'drawing' }
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

{ #category : 'drawing' }
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

{ #category : 'accessing' }
RubExtraSelectionDecorator >> findMatchColor [
	"The theme color of the find matches (see ThemeSettings>>findReplaceSelectionColor)."

	^ self theme currentSettings findReplaceSelectionColor
]

{ #category : 'accessing' }
RubExtraSelectionDecorator >> findRegex [
	^ self paragraph textArea ifNotNil: [ :textArea | textArea findRegex ]
]

{ #category : 'accessing' }
RubExtraSelectionDecorator >> findReplaceService [
	^ self paragraph textArea ifNotNil: [ :textArea | textArea findReplaceService ]
]

{ #category : 'testing' }
RubExtraSelectionDecorator >> hasValidSelection [

	| paragraph |

	paragraph := self paragraph ifNil: [ ^false ].

	^paragraph selectionStart isNotNil and: [
		paragraph selectionStop isNotNil and: [
			paragraph selectionStart ~= paragraph selectionStop
		]
	]
]

{ #category : 'accessing' }
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
