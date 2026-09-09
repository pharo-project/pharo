"
I display some detail information for a selected ECEntry in the ECMenuMorph.
The content I show, is provided by an ECDetailContentProvider subclass. 

"
Class {
	#name : 'NECDetailMorph',
	#superclass : 'Morph',
	#instVars : [
		'title',
		'description',
		'arrowPosition',
		'label'
	],
	#category : 'NECompletion-Morphic',
	#package : 'NECompletion-Morphic'
}

{ #category : 'private' }
NECDetailMorph class >> height [
	^ NECMenuMorph itemHeight * 15.5
]

{ #category : 'private' }
NECDetailMorph class >> width [
	^ NECMenuMorph itemWidth * 2.0
]

{ #category : 'drawing' }
NECDetailMorph >> bounds [
	^ super bounds topLeft extent: self class width @ self class height
]

{ #category : 'drawing' }
NECDetailMorph >> contentBounds [

	| factor rectangle |
	factor := self itemHeight + 10.
	rectangle := self bounds top: self bounds top + 3.
	rectangle := rectangle left: rectangle left + (factor * 2.0).
	rectangle := rectangle bottom: rectangle bottom.
	^ rectangle
]

{ #category : 'drawing' }
NECDetailMorph >> defaultColor [

	^ NECMenuMorph backgroundColor
]

{ #category : 'drawing' }
NECDetailMorph >> descriptionBounds [

	
	^ self contentBounds top: self contentBounds top + 40
]

{ #category : 'drawing' }
NECDetailMorph >> drawArrowOn: aCanvas [

	| point factor polygon |
	factor := self itemHeight.
	point := arrowPosition.
	polygon := OrderedCollection new.
	polygon add: point.
	polygon add: (point := point translateBy: (factor / 2) @ 0).
	polygon add: (point := point translateBy: 0 @ (factor * -0.5)).
	polygon add: (point := point translateBy: factor @ factor).
	polygon add: (point := point translateBy: (factor * -1) @ factor).
	polygon add: (point := point translateBy: 0 @ (factor * -0.5)).
	polygon add: (point := point translateBy: (factor * -0.5) @ 0).
	aCanvas drawPolygon: polygon fillStyle: self scrollColor
]

{ #category : 'drawing' }
NECDetailMorph >> drawOn: aCanvas [

	super drawOn: aCanvas.
	arrowPosition ifNotNil: [ self drawArrowOn: aCanvas ]
]

{ #category : 'accessing' }
NECDetailMorph >> entryDescription: anECEntryDescription [

	title
		contents: (anECEntryDescription title
				ifNil: [
					description bounds: self titleBounds.
					String new]
				ifNotNil: [
					description bounds: self descriptionBounds.
					anECEntryDescription title]).
	description contentsWrapped: anECEntryDescription description.
	label contents: anECEntryDescription label.
]

{ #category : 'initialization' }
NECDetailMorph >> initialize [
	| childBounds |
	super initialize.

	self borderStyle: (BorderStyle color: Color gray width: 1).
	childBounds := self contentBounds.
	label := StringMorph
		contents: ''
		font: self messageFont.
		
	label bounds: childBounds.
	self addMorph: label.
	
	title := StringMorph
		contents: ''
		font: self titleFont.
	title bounds: self titleBounds.
	self addMorph: title.
	
	description := TextMorph new.
	description autoFit: false.
	description bounds: self descriptionBounds.
	description borderWidth: 0.
	self addMorph: description
]

{ #category : 'visual parameters' }
NECDetailMorph >> itemHeight [

	^ NECMenuMorph itemHeight
]

{ #category : 'fonts' }
NECDetailMorph >> messageFont [

	^ NECMenuMorph messageFont
]

{ #category : 'drawing' }
NECDetailMorph >> position: aPoint menuWidth: anInteger [

	| y x |
	arrowPosition := aPoint.
	y := aPoint y + self class height.
	y := y > self currentWorld height
		     ifTrue: [ self currentWorld height - self class height ]
		     ifFalse: [ aPoint y - self itemHeight ].
	x := aPoint x.
	x := x + self class width > self currentWorld width
		     ifTrue: [
			     arrowPosition := (self positionOnLeft: anInteger) @ aPoint y.
			     arrowPosition x ]
		     ifFalse: [ x ].
	self position: x @ y
]

{ #category : 'private' }
NECDetailMorph >> positionOnLeft: anInteger [

	^ arrowPosition x - self class width - anInteger
]

{ #category : 'visual parameters' }
NECDetailMorph >> scrollColor [

	^ NECMenuMorph scrollColor
]

{ #category : 'drawing' }
NECDetailMorph >> titleBounds [
	^ self contentBounds top: self contentBounds top + 20
]

{ #category : 'fonts' }
NECDetailMorph >> titleFont [ 

	^ NECMenuMorph titleFont
]
