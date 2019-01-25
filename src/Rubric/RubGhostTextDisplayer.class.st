Class {
	#name : #RubGhostTextDisplayer,
	#superclass : #RubScrolledTextExtraSurface,
	#instVars : [
		'ghostText',
		'textColor'
	],
	#category : #'Rubric-Editing-Widgets'
}

{ #category : #querying }
RubGhostTextDisplayer class >> key [
	^ #ghostText
]

{ #category : #accessing }
RubGhostTextDisplayer >> bold [
	self handleTextChangeWhile: [ghostText selectAll; bold; deselect]

]

{ #category : #alignment }
RubGhostTextDisplayer >> centered [
	self handleTextChangeWhile: [ ghostText centered ]

]

{ #category : #accessing }
RubGhostTextDisplayer >> computedPositionFrom: aRectangle [
	^ self scrollBounds topLeft

]

{ #category : #accessing }
RubGhostTextDisplayer >> computedWidthFrom: aRectangle [
	^  self scrollBounds width

]

{ #category : #'initialize-release' }
RubGhostTextDisplayer >> defaultTextColor [
	^ self theme ghostTextColor
]

{ #category : #geometry }
RubGhostTextDisplayer >> extent: aPoint [
	super extent: aPoint.
	self showHideGhostText
]

{ #category : #accessing }
RubGhostTextDisplayer >> font: aFont [
	self handleTextChangeWhile: [ghostText textFont: aFont]
]

{ #category : #accessing }
RubGhostTextDisplayer >> fontName: aFontName pointSize: anInteger [
	self
		handleTextChangeWhile: [ 
			ghostText
				beEditableWhile: [ 
					ghostText fontName: aFontName pointSize: anInteger.
					ghostText compose.
					self changed ] ]
]

{ #category : #accessing }
RubGhostTextDisplayer >> fontPointSize: anInteger [
	self handleTextChangeWhile: [ghostText fontPointSize: anInteger]
]

{ #category : #accessing }
RubGhostTextDisplayer >> handleTextChangeWhile: aBlock [ 
	aBlock value.
	self showHideGhostText
]

{ #category : #alignment }
RubGhostTextDisplayer >> leftFlush [
	self handleTextChangeWhile: [ ghostText leftFlush ]

]

{ #category : #accessing }
RubGhostTextDisplayer >> level [
	^ SmallInteger maxVal  - 1
]

{ #category : #accessing }
RubGhostTextDisplayer >> manageLayoutInBounds: aRectangle [
	| ret |
	self handleTextChangeWhile: [ ret := super manageLayoutInBounds: aRectangle ].
	^ ret
]

{ #category : #'submorphs-accessing' }
RubGhostTextDisplayer >> noteNewOwner: aMorph [
	super noteNewOwner: aMorph.
	ghostText := self paragraphProvider defaultGhostTextMorph.
	textColor := self defaultTextColor. 
	self textArea announcer when: MorphGotFocus send: #showHideGhostText to: self.	
	self textArea announcer when: MorphLostFocus send: #showHideGhostText to: self.
]

{ #category : #alignment }
RubGhostTextDisplayer >> rightFlush [
	self
		handleTextChangeWhile: [ ghostText rightFlush ]

]

{ #category : #accessing }
RubGhostTextDisplayer >> setTextWith: aText [
	ghostText setTextWith: aText asText.
	ghostText  textColor: self textColor

]

{ #category : #private }
RubGhostTextDisplayer >> showHideGhostText [
	self textArea text
		ifNotEmpty: [ self removeAllMorphs ]
		ifEmpty: [ 
			self paragraphProvider configureGhostText: ghostText.
			self addMorph: ghostText ]
]

{ #category : #'event handling' }
RubGhostTextDisplayer >> textChanged [ 
	self showHideGhostText.
	super textChanged

]

{ #category : #accessing }
RubGhostTextDisplayer >> textColor [
	^ textColor
]

{ #category : #accessing }
RubGhostTextDisplayer >> textColor: aColor [
	textColor := aColor.
	self
		handleTextChangeWhile: [ ghostText textColor: aColor ]
]

{ #category : #accessing }
RubGhostTextDisplayer >> updateMarginsWith: aMargin [
	self
		handleTextChangeWhile: [ ghostText updateMarginsWith: aMargin]

]

{ #category : #accessing }
RubGhostTextDisplayer >> updateTextWith: aText [
	self
		handleTextChangeWhile: [ 
			ghostText updateTextWith: aText.
			ghostText textColor: self textColor ]
]
