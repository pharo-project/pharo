"
A scroll pane to show a miller column list.
"
Class {
	#name : #MillerScrollPane,
	#superclass : #GeneralScrollPane,
	#instVars : [
		'layoutChangeAction',
		'updating',
		'firstShowingMorph',
		'keepShowingPage'
	],
	#category : #'Spec2-Adapters-Morphic-Layout'
}

{ #category : #private }
MillerScrollPane >> basicShowMorph: aMorph [

	scroller offset: aMorph topLeft
]

{ #category : #scrollbars }
MillerScrollPane >> hScrollbarShowNever [

	super hScrollbarShowNever.
	"Since I am showing a scroll bar, I do not need to keep the page."
	self keepShowingPage: true
]

{ #category : #scrollbars }
MillerScrollPane >> hScrollbarShowWhenNeeded [

	super hScrollbarShowWhenNeeded.
	"Since I am showing a scroll bar, I do not need to keep the page."
	self keepShowingPage: false
]

{ #category : #initialization }
MillerScrollPane >> initialize [

	super initialize.
	updating := false.
	keepShowingPage := false
]

{ #category : #testing }
MillerScrollPane >> isUpdating [

	^ updating ifNil: [ updating := false ]
]

{ #category : #private }
MillerScrollPane >> keepShowingPage: aBoolean [

	keepShowingPage := aBoolean.
	keepShowingPage ifFalse: [ 
		firstShowingMorph := nil ]
]

{ #category : #events }
MillerScrollPane >> layoutChanged [

	self isUpdating ifTrue: [ ^ self ].
	updating := true.
	[ 
	super layoutChanged.
	layoutChangeAction ifNotNil: [ layoutChangeAction value ] ] ensure: [ 
		updating := false ]
]

{ #category : #events }
MillerScrollPane >> onLayoutChange: aBlock [

	layoutChangeAction := aBlock
]

{ #category : #accessing }
MillerScrollPane >> showMorph: aMorph [
	
	keepShowingPage 
		ifTrue: [ firstShowingMorph := aMorph ].
	self basicShowMorph: aMorph
]

{ #category : #updating }
MillerScrollPane >> updateScrollbars [

	super updateScrollbars.
	firstShowingMorph ifNotNil: [ 
		scroller offset: firstShowingMorph topLeft ]
]

{ #category : #scrollbars }
MillerScrollPane >> vScrollbarNeeded [
	"Return whether the vertical scrollbar is needed."

	self vScrollbar showsAlways ifTrue: [ ^ true ].
	self vScrollbar showsNever ifTrue: [ ^ false ].	
 	keepShowingPage ifNil: [ ^ super vScrollbarNeeded ].
	
	^ self scrollTarget height > self innerBounds height
]
