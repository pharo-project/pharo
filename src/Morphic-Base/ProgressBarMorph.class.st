"
Instances of this morph get used by SystemProgressMorph or a JobProgressBar to display a bar (i.e., the rectangular part of a progress bar).

Here is an example of how to use it. 

| p |
p := ProgressBarMorph from: 0 to: 200.
p extent: 200@20.
p openInWorld.

[
	(1 to: 200) do: [ :i | p value: i.  (Delay forMilliseconds: 10) wait ].
	p delete ] fork


| p |
p := ProgressBarMorph from: 0 to: 200.
p extent: 600@7.
p openInWorld.

[
	(1 to: 200) do: [ :i | p value: i.  (Delay forMilliseconds: 5) wait ].
	p delete ] fork
"
Class {
	#name : #ProgressBarMorph,
	#superclass : #BorderedMorph,
	#traits : 'TAbleToRotate',
	#classTraits : 'TAbleToRotate classTrait',
	#instVars : [
		'start',
		'end',
		'value',
		'height',
		'width',
		'cachedWidth'
	],
	#classVars : [
		'DefaultHeight',
		'DefaultWidth'
	],
	#category : #'Morphic-Base-ProgressBar'
}

{ #category : #defaults }
ProgressBarMorph class >> defaultHeight: aNumber [

	DefaultHeight := aNumber
]

{ #category : #defaults }
ProgressBarMorph class >> defaultWidth: aNumber [
	
	DefaultWidth := aNumber.

]

{ #category : #'instance creation' }
ProgressBarMorph class >> from: startNumber to: endNumber [

	^ self new
		startAt: startNumber;
		endAt: endNumber.
]

{ #category : #'class initialization' }
ProgressBarMorph class >> initialize [
	
	self defaultWidth: 120.
	self defaultHeight: 10 
]

{ #category : #private }
ProgressBarMorph >> barFillStyle [
	"Answer the fillStyle for the bar if present or
	the theme menuTitleColor otherwise."

	^ self 
		valueOfProperty: #barFillStyle 
		ifAbsent: [ self theme menuTitleColor ]
]

{ #category : #private }
ProgressBarMorph >> barFillStyle: aFillStyle [
	"Set the fillStyle for the bar."

	^self setProperty: #barFillStyle toValue: aFillStyle
]

{ #category : #private }
ProgressBarMorph >> completedWidth [

	| range fraction progress |
	range := end - start.
	progress := value - start.
	fraction := progress / range.
	^ (fraction * self totalBarWidth) truncated.
]

{ #category : #accessing }
ProgressBarMorph >> decrement [

	self value: self value - 1.
]

{ #category : #drawing }
ProgressBarMorph >> drawOn: aCanvas [
	"Draw the receiver with the fill style for the bar, clipping to the inner bounds."

	| area |
	area := self innerBounds.
	aCanvas fillRectangle: area fillStyle: self fillStyle.
	value > 0
		ifTrue: [ 
			cachedWidth := self completedWidth.
			area := area origin extent: cachedWidth @ area height.
			aCanvas fillRectangle: area fillStyle: self barFillStyle ].
	self borderStyle frameRectangle: self bounds on: aCanvas
]

{ #category : #private }
ProgressBarMorph >> endAt: aNumber [ 

	end := aNumber.
]

{ #category : #geometry }
ProgressBarMorph >> extent: aPoint [
	"Update the bar fillStyle if appropriate."
	
	super extent: aPoint.
	self fillStyle isOrientedFill ifTrue: [
		self fillStyle: (self theme progressBarFillStyleFor: self)].
	self barFillStyle isOrientedFill ifTrue: [
		self barFillStyle: ( self theme progressBarProgressFillStyleFor: self)]
]

{ #category : #accessing }
ProgressBarMorph >> increment [

	self value: self value + 1.
]

{ #category : #initialization }
ProgressBarMorph >> initialize [
	"Initialize the receiver from the current theme."
	
	super initialize.
	value := 0.
	start := 0.
	end := 100.
	height := DefaultHeight.
	width := DefaultWidth.
	cachedWidth := 0.
	self 
		fillStyle: (self theme progressBarFillStyleFor: self);
		borderStyle: (self theme progressBarBorderStyleFor: self);
		barFillStyle: (self theme progressBarProgressFillStyleFor: self);
		extent: width@height  + (2 * self borderWidth).
]

{ #category : #private }
ProgressBarMorph >> privateMoveBy: delta [ 
	"Update the bar fillStyle if appropriate."
	
	| fill |
	super privateMoveBy: delta.
	fill := self barFillStyle.
	fill isOrientedFill ifTrue: [fill origin: fill origin + delta]
]

{ #category : #private }
ProgressBarMorph >> startAt: aNumber [ 

	value := start := aNumber.
]

{ #category : #private }
ProgressBarMorph >> totalBarWidth [

	^ self width - (2 * self borderWidth).
]

{ #category : #accessing }
ProgressBarMorph >> value [
	
	^ value.
]

{ #category : #accessing }
ProgressBarMorph >> value: aNumber [
	|testWidth|
	value := aNumber.
	testWidth := self completedWidth.
	testWidth ~= cachedWidth ifTrue: [  
		cachedWidth := testWidth.
		self changed]
]
