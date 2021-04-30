"
I am a decorator for the paragraph of a text area. I can be used to draw something on the paragraph 
"
Class {
	#name : #RubParagraphDecorator,
	#superclass : #Object,
	#instVars : [
		'next'
	],
	#category : #'Rubric-Editing-Core'
}

{ #category : #querying }
RubParagraphDecorator class >> classOfDecoratorNamed: aKey [ 
	^ self allSubclasses detect: [ :cls | cls key = aKey ] ifNone: [  ]
]

{ #category : #testing }
RubParagraphDecorator class >> isUsed [
	"all my subclasses might be used via #classOfDecoratorNamed:"
	^self name = 'RubParagraphDecorator'
		ifTrue: [ super isUsed ]
		ifFalse: [ true ]
]

{ #category : #querying }
RubParagraphDecorator class >> key [
	^ self subclassResponsibility 
]

{ #category : #'instance creation' }
RubParagraphDecorator class >> next: aDecoratorOrAParagraph [
	^ self new next: aDecoratorOrAParagraph 
]

{ #category : #delegating }
RubParagraphDecorator >> aboutToBeUnplugged [ 
	^self paragraph aboutToBeUnplugged 
]

{ #category : #drawing }
RubParagraphDecorator >> canDrawDecoratorsOn: aCanvas [

	^self drawingEnabled and: [ 
		self selectionStart isNotNil and: [ 
			self selectionStop isNotNil
		]
	]
	
]

{ #category : #delegating }
RubParagraphDecorator >> characterBlockAtPoint: aPoint [

	^self paragraph characterBlockAtPoint: aPoint 
]

{ #category : #delegating }
RubParagraphDecorator >> characterBlockForIndex: index [
	^self paragraph characterBlockForIndex: index
]

{ #category : #delegating }
RubParagraphDecorator >> click: anEvent for: model controller: editor [

	^self paragraph click: anEvent for: model controller: editor
]

{ #category : #delegating }
RubParagraphDecorator >> closingDelimiters [

	^self paragraph closingDelimiters
]

{ #category : #delegating }
RubParagraphDecorator >> compose [ 
	^self paragraph compose 
]

{ #category : #delegating }
RubParagraphDecorator >> container [
	^self paragraph container 
]

{ #category : #delegating }
RubParagraphDecorator >> container: atextContainerOrRectangle [
	^self paragraph container: atextContainerOrRectangle 
]

{ #category : #querying }
RubParagraphDecorator >> decoratorNamed: aKey [
	^ self key = aKey
		ifTrue: [ self ]
		ifFalse: [ next decoratorNamed: aKey ]
]

{ #category : #delegating }
RubParagraphDecorator >> defaultCharacterBlock [ 
	^self paragraph defaultCharacterBlock 
]

{ #category : #delegating }
RubParagraphDecorator >> defaultFontChange [

	^self paragraph defaultFontChange
]

{ #category : #delegating }
RubParagraphDecorator >> disableDrawingWhile: aBlock [
	^self paragraph disableDrawingWhile: aBlock
]

{ #category : #'error handling' }
RubParagraphDecorator >> doesNotUnderstand: aMessage [
	^ [ aMessage sendTo: next ]
		on: MessageNotUnderstood
		do: [ super doesNotUnderstand: aMessage ]
]

{ #category : #drawing }
RubParagraphDecorator >> drawOn: aCanvas [ 
	"do nothing"
]

{ #category : #drawing }
RubParagraphDecorator >> drawOn: aCanvas using: aDisplayScanner at: aPosition [

	(self canDrawDecoratorsOn: aCanvas) ifFalse: [ ^self ].
	
	self drawOn: aCanvas.
	next drawOn: aCanvas using: aDisplayScanner at: aPosition
]

{ #category : #drawing }
RubParagraphDecorator >> drawOnAthensCanvas: anAthensCanvas [
	"do nothing by default"
]

{ #category : #drawing }
RubParagraphDecorator >> drawOnAthensCanvas: aCanvas bounds: aRectangle color: aColor [

   (self canDrawDecoratorsOn: aCanvas) ifFalse: [ ^self ].

	self drawOnAthensCanvas: aCanvas.
	next drawOnAthensCanvas: aCanvas bounds: aRectangle color: aColor
]

{ #category : #delegating }
RubParagraphDecorator >> drawingEnabled [

	^self paragraph drawingEnabled
]

{ #category : #delegating }
RubParagraphDecorator >> extent [ 
	^self paragraph extent 
]

{ #category : #querying }
RubParagraphDecorator >> hasDecorator: aDecorator [
	^ self = aDecorator or: [ next hasDecorator: aDecorator ]
]

{ #category : #querying }
RubParagraphDecorator >> hasDecoratorNamed: aKey [
	^ self key = aKey or: [ next hasDecoratorNamed: aKey ]
]

{ #category : #querying }
RubParagraphDecorator >> key [
	^self class key
]

{ #category : #delegating }
RubParagraphDecorator >> lineIndexForPoint: aPoint [

	^self paragraph lineIndexForPoint: aPoint 
]

{ #category : #delegating }
RubParagraphDecorator >> lineIndexOfCharacterIndex: characterIndex [ 
	^self paragraph lineIndexOfCharacterIndex: characterIndex 
]

{ #category : #delegating }
RubParagraphDecorator >> lines [ 
	^self paragraph lines 
]

{ #category : #delegating }
RubParagraphDecorator >> margins [

	^self paragraph margins
]

{ #category : #delegating }
RubParagraphDecorator >> minimumExtent [

	^self paragraph minimumExtent
]

{ #category : #delegating }
RubParagraphDecorator >> model [
	^self paragraph model 
]

{ #category : #delegating }
RubParagraphDecorator >> move: anEvent for: model controller: editor [

	^self paragraph move: anEvent for: model controller: editor
]

{ #category : #delegating }
RubParagraphDecorator >> moveBy: aPoint [

	^self paragraph moveBy: aPoint 
]

{ #category : #accessing }
RubParagraphDecorator >> next [
	^ next
]

{ #category : #accessing }
RubParagraphDecorator >> next: anObject [
	next := anObject
]

{ #category : #delegating }
RubParagraphDecorator >> numberOfLines [ 
	^self paragraph numberOfLines 
]

{ #category : #delegating }
RubParagraphDecorator >> numberOfPhysicalLines [

	^self paragraph numberOfPhysicalLines
]

{ #category : #delegating }
RubParagraphDecorator >> openingDelimiters [

	^self paragraph openingDelimiters
]

{ #category : #delegating }
RubParagraphDecorator >> paragraph [
	^next paragraph 
]

{ #category : #delegating }
RubParagraphDecorator >> replaceFrom: start to: stop with: aText [
	^self paragraph replaceFrom: start to: stop with: aText 
]

{ #category : #delegating }
RubParagraphDecorator >> selection [

	^self paragraph selection
]

{ #category : #delegating }
RubParagraphDecorator >> selectionRects [

	^self paragraph selectionRects
]

{ #category : #delegating }
RubParagraphDecorator >> selectionStart [

	^self paragraph selectionStart
]

{ #category : #delegating }
RubParagraphDecorator >> selectionStop [

	^self paragraph selectionStop
]

{ #category : #delegating }
RubParagraphDecorator >> string [

	^self paragraph string
]

{ #category : #delegating }
RubParagraphDecorator >> tabWidth: anInteger [

	^self paragraph tabWidth: anInteger
]

{ #category : #delegating }
RubParagraphDecorator >> text [ 

	^self paragraph text
]

{ #category : #delegating }
RubParagraphDecorator >> text: aText [

	^self paragraph text: aText
]

{ #category : #delegating }
RubParagraphDecorator >> textArea [

	^self paragraph textArea
]

{ #category : #delegating }
RubParagraphDecorator >> textArea: aClient [

	^self paragraph textArea: aClient
]

{ #category : #delegating }
RubParagraphDecorator >> textStyle [

	^self paragraph textStyle
]

{ #category : #delegating }
RubParagraphDecorator >> theme [

	^self paragraph theme
]

{ #category : #initialization }
RubParagraphDecorator >> unplug [
	next
		ifNotNil: [ 
			next unplug.
			next := nil ].
	super unplug
]

{ #category : #delegating }
RubParagraphDecorator >> verticesFrom: firstIndex to: lastIndex [

	^self paragraph verticesFrom: firstIndex to: lastIndex
]

{ #category : #accessing }
RubParagraphDecorator >> visibleStringFrom: topLeft to: bottomRight [
	| topLeftBlk bottomRightBlk startIdx stopIdx string  |
	topLeftBlk := self characterBlockAtPoint: topLeft.
	bottomRightBlk := self characterBlockAtPoint: bottomRight.
	startIdx := topLeftBlk stringIndex.
	stopIdx := bottomRightBlk stringIndex.
	string := self text string.
	^ self text string copyFrom: startIdx to: (stopIdx min: string size)

]

{ #category : #accessing }
RubParagraphDecorator >> withoutDecorator [

	"for compatibility with the RubParagraph API"

	^ self paragraph

]
