"
Description
--------------------

I am the adapter used to bridge a URLPresenter and a morphic
"
Class {
	#name : #SpMorphicLinkAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #accessing }
SpMorphicLinkAdapter >> action [
	^ self model action
]

{ #category : #factory }
SpMorphicLinkAdapter >> buildWidget [
	"Since Pharo does not yet have a real morph for URL, I create my own. Maybe later we will have real links in Morphic?"

	| aMorph |
	
	aMorph := StringMorph new
		contents: self getLabelText;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		vResizing: #shrinkWrap;
		hResizing: #spaceFill;
		setBalloonText: self getHelpText;
		color: self urlColor;
		on: #mouseEnter send: #mouseEnter:from: to: self;
		on: #mouseLeave send: #mouseLeave:from: to: self;
		on: #mouseDown send: #mouseDown:from: to: self;
		on: #mouseUp send: #mouseUp:from: to: self;
		yourself.	";
		emphasis: (self emphasisCodeFor: self model emphasis)"
		
	self presenter whenLabelChangedDo: [ :newLabel | aMorph contents: (newLabel ifNotNil: [ :aString | aString localizedForPresenter: self presenter ]) ].
	
	^ aMorph
]

{ #category : #private }
SpMorphicLinkAdapter >> getHelpText [

	^ self presenter help ifNotNil: [ :aString | aString localizedForPresenter: self presenter ]
]

{ #category : #private }
SpMorphicLinkAdapter >> getLabelText [

	^ self presenter label ifNotNil: [ :aString | aString localizedForPresenter: self presenter ]
]

{ #category : #accessing }
SpMorphicLinkAdapter >> label [
	^ self getLabelText
]

{ #category : #'event handling' }
SpMorphicLinkAdapter >> mouseDown: anEvent from: aMorph [
	aMorph color: self urlDuringClickColor
]

{ #category : #'event handling' }
SpMorphicLinkAdapter >> mouseEnter: anEvent from: aMorph [
	aMorph color: self urlHoverColor.
	self currentHand showTemporaryCursor: Cursor webLink
]

{ #category : #'event handling' }
SpMorphicLinkAdapter >> mouseLeave: anEvent from: aMorph [
	aMorph color: self urlColor.
	self currentHand showTemporaryCursor: nil
]

{ #category : #'event handling' }
SpMorphicLinkAdapter >> mouseUp: anEvent from: aMorph [
	aMorph color: self urlHoverColor.
	self action value
]

{ #category : #'accessing colors' }
SpMorphicLinkAdapter >> urlColor [
	^ self color ifNil: [ self theme urlColor ]
]

{ #category : #'accessing colors' }
SpMorphicLinkAdapter >> urlDuringClickColor [
	^ self urlHoverColor darker
]

{ #category : #'accessing colors' }
SpMorphicLinkAdapter >> urlHoverColor [
	^ self urlColor darker
]
