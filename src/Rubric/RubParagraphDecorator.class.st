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

{ #category : #querying }
RubParagraphDecorator class >> key [
	^ self subclassResponsibility 
]

{ #category : #'instance creation' }
RubParagraphDecorator class >> next: aDecoratorOrAParagraph [
	^ self new next: aDecoratorOrAParagraph 
]

{ #category : #drawing }
RubParagraphDecorator >> canDrawDecoratorsOn: aCanvas [

	^self drawingEnabled and: [ 
		self selectionStart isNotNil and: [ 
			self selectionStop isNotNil
		]
	]
	
]

{ #category : #querying }
RubParagraphDecorator >> decoratorNamed: aKey [
	^ self key = aKey
		ifTrue: [ self ]
		ifFalse: [ next decoratorNamed: aKey ]
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

{ #category : #accessing }
RubParagraphDecorator >> next [
	^ next
]

{ #category : #accessing }
RubParagraphDecorator >> next: anObject [
	next := anObject
]

{ #category : #'instead of DNU' }
RubParagraphDecorator >> paragraph [
	"this method is here to find the paragraph in the chain, instead of relying on implementing #doesNotUnderstand: !!!"

	| p |
	
	p := next.
	
	[ p  isNotNil and: [ p isKindOf: RubParagraph ] ] whileFalse: [ 
		p := p next.
	].

	^p
	
	
]

{ #category : #'instead of DNU' }
RubParagraphDecorator >> text [ 

	^self paragraph text
]

{ #category : #initialization }
RubParagraphDecorator >> unplug [
	next
		ifNotNil: [ 
			next unplug.
			next := nil ].
	super unplug
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
