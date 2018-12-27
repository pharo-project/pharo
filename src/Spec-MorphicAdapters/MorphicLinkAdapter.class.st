"
Description
--------------------

I am the adapter used to bridge a URLPresenter and a morphic
"
Class {
	#name : #MorphicLinkAdapter,
	#superclass : #AbstractMorphicAdapter,
	#instVars : [
		'previousCursor'
	],
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #factory }
MorphicLinkAdapter >> buildWidget [
	"Since Pharo does not yet have a real morph for URL, I create my own. Maybe later we will have real links in Morphic?"

	^ StringMorph new
		contents: self label;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		vResizing: #shrinkWrap;
		hResizing: #spaceFill;
		setBalloonText: self help;
		color: self urlColor;
		on: #mouseEnter send: #mouseEnter:from: to: self;
		on: #mouseLeave send: #mouseLeave:from: to: self;
		on: #mouseDown send: #mouseDown:from: to: self;
		on: #mouseUp send: #mouseUp:from: to: self;
		yourself	";
		emphasis: (self emphasisCodeFor: self model emphasis)"
]

{ #category : #accessing }
MorphicLinkAdapter >> label [
	^ self model label
]

{ #category : #'event handling' }
MorphicLinkAdapter >> mouseDown: anEvent from: aMorph [
	aMorph color: self urlDuringClickColor
]

{ #category : #'event handling' }
MorphicLinkAdapter >> mouseEnter: anEvent from: aMorph [
	aMorph color: self urlHoverColor.
	self currentHand showTemporaryCursor: Cursor webLink
]

{ #category : #'event handling' }
MorphicLinkAdapter >> mouseLeave: anEvent from: aMorph [
	aMorph color: self urlColor.
	self currentHand showTemporaryCursor: nil
]

{ #category : #'event handling' }
MorphicLinkAdapter >> mouseUp: anEvent from: aMorph [
	aMorph color: self urlHoverColor.
	WebBrowser openOn: self url
]

{ #category : #accessing }
MorphicLinkAdapter >> url [
	^ self model url
]

{ #category : #'accessing colors' }
MorphicLinkAdapter >> urlColor [
	^ self color ifNil: [ self theme urlColor ]
]

{ #category : #'accessing colors' }
MorphicLinkAdapter >> urlDuringClickColor [
	^ self urlHoverColor darker
]

{ #category : #'accessing colors' }
MorphicLinkAdapter >> urlHoverColor [
	^ self urlColor darker
]
