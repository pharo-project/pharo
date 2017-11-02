"
I display some detail information for a selected ECEntry in the ECMenuMorph.
The content I show, is provided by an ECDetailContentProvider subclass. 

"
Class {
	#name : #NECDetailMorph,
	#superclass : #Morph,
	#instVars : [
		'title',
		'description',
		'arrowPosition',
		'label'
	],
	#category : 'NECompletion-View'
}

{ #category : #private }
NECDetailMorph class >> height [
	^ NECMenuMorph itemHeight * 15.5
]

{ #category : #private }
NECDetailMorph class >> width [
	^ NECMenuMorph itemWidth * 2.0
]

{ #category : #drawing }
NECDetailMorph >> bounds [
	^ super bounds topLeft extent: self class width @ self class height
]

{ #category : #drawing }
NECDetailMorph >> contentBounds [
	| factor rectangle |
	factor := self itemHeight.
	rectangle := self bounds top: self bounds top + 3.
	rectangle := rectangle left: rectangle left + (factor * 2.0).
	rectangle := rectangle bottom: rectangle bottom - factor.
	^ rectangle
]

{ #category : #drawing }
NECDetailMorph >> defaultColor [
	^ NECMenuMorph backgroundColor. 
]

{ #category : #accessing }
NECDetailMorph >> descriptionBounds [
	^ self contentBounds top: self contentBounds top + 30
]

{ #category : #drawing }
NECDetailMorph >> drawArrowOn: aCanvas [
	| point factor poligon |
	factor := self itemHeight.
	point := arrowPosition.
	poligon := OrderedCollection new.
	poligon add: point.
	poligon add: (point := point translateBy: (factor / 2) @ 0).
	poligon add: (point := point translateBy: 0 @ (factor * -0.5)).
	poligon add: (point := point translateBy: factor @ factor).
	poligon add: (point := point translateBy: (factor * -1) @ factor).
	poligon add: (point := point translateBy: 0 @ (factor * -0.5)).
	poligon add: (point := point translateBy: (factor * -0.5) @ 0).
	aCanvas drawPolygon: poligon fillStyle: self scrollColor
]

{ #category : #drawing }
NECDetailMorph >> drawMessageOn: aCanvas [
	| factor rectangle width browseMessage |
	factor := self itemHeight.
	rectangle := self bounds top: self bounds bottom - factor.
	rectangle := rectangle left: self contentBounds left.
	aCanvas 
		line: rectangle topLeft 
		to: rectangle topRight
		color: Color darkGray.
	rectangle := rectangle top: rectangle top + 1.
	aCanvas
		drawString: '<- close detail'
		in: rectangle
		font: self messageFont
		color: Color darkGray.
	browseMessage := 'browse ->'.
	width := self messageFont widthOfString: browseMessage.
	aCanvas
		drawString: browseMessage
		in: (rectangle left: rectangle right - width)
		font: self messageFont
		color: Color darkGray
]

{ #category : #drawing }
NECDetailMorph >> drawOn: aCanvas [ 
	super drawOn: aCanvas.
	arrowPosition ifNotNil: [
		self drawArrowOn: aCanvas.
		self drawMessageOn: aCanvas]
]

{ #category : #accessing }
NECDetailMorph >> entryDescription: anECEntryDescription [ 
	| categoryContents entryDescription |
	entryDescription := anECEntryDescription.
	title
		contents: (entryDescription title
				ifNil: [
					description bounds: self titleBounds.
					String new]
				ifNotNil: [
					description bounds: self descriptionBounds.
					entryDescription title]).
	description contentsWrapped: entryDescription description.
	categoryContents := entryDescription label.
	label contents: categoryContents
]

{ #category : #initialization }
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
		font: NECMenuMorph titleFont.
	title bounds: self titleBounds.
	self addMorph: title.
	description := TextMorph new.
	description autoFit: false.
	description bounds: self descriptionBounds.
	description borderWidth: 0.
	self addMorph: description
]

{ #category : #drawing }
NECDetailMorph >> itemHeight [
	^ NECMenuMorph itemHeight
]

{ #category : #drawing }
NECDetailMorph >> messageFont [
	^ NECMenuMorph messageFont
]

{ #category : #drawing }
NECDetailMorph >> position: aPoint menuWidth: anInteger [ 
	| y x |
	arrowPosition := aPoint.
	y := aPoint y + self class height.
	y := y > Display height
				ifTrue: [Display height - self class height]
				ifFalse: [aPoint y - self itemHeight].
	x := aPoint x.
	x := x + self class width > Display width
				ifTrue: [
					arrowPosition := (self positionOnLeft: anInteger) @ aPoint y.
					arrowPosition x]
				ifFalse: [x].
	self position: x @ y
]

{ #category : #private }
NECDetailMorph >> positionOnLeft: anInteger [ 
	^ arrowPosition x - self class width - anInteger 
]

{ #category : #drawing }
NECDetailMorph >> scrollColor [
	^ NECMenuMorph scrollColor
]

{ #category : #accessing }
NECDetailMorph >> titleBounds [
	^ self contentBounds top: self contentBounds top + 10
]
