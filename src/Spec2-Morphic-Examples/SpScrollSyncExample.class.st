"
I am an example that exposes to the world how to use a ScrollPane announcement to have a nice morph being in sync with the current vertical scrollbar position.

self example
"
Class {
	#name : #SpScrollSyncExample,
	#superclass : #SpPresenter,
	#instVars : [
		'text',
		'sideRuler',
		'currentVerticalScrollPosition'
	],
	#category : #'Spec2-Morphic-Examples'
}

{ #category : #specs }
SpScrollSyncExample class >> defaultSpec [
	<spec>
	^ SpBoxLayout newHorizontal
		add: #sideRuler withConstraints: [ :constraints | constraints width: 50 ];
		add: #text;
		yourself
]

{ #category : #example }
SpScrollSyncExample class >> example [
	<sampleInstance>
	^ self new openWithSpec
]

{ #category : #specs }
SpScrollSyncExample class >> title [

	^ self name
]

{ #category : #accessing }
SpScrollSyncExample >> currentVerticalScrollPosition [
	^ currentVerticalScrollPosition
]

{ #category : #layouts }
SpScrollSyncExample >> defaultSpecLayout [
	<spec>
	
	^ SpBoxLayout newVertical
		add: #sideRuler;
		add: #text;
		yourself
]

{ #category : #initialization }
SpScrollSyncExample >> initialize [
	super initialize.
	currentVerticalScrollPosition := 0
]

{ #category : #initialization }
SpScrollSyncExample >> initializePresenters [
	text := self newText.
	sideRuler := self newMorph.

	sideRuler
		morph:
			(Morph new
				color: Color yellow;
				extent: 50 @ 500;
				useGradientFill;
				hResizing: #spaceFill;
				vResizing: #spaceFill;
				yourself).

	text whenScrollValueChangedDo: [ :newPosition | self syncMorphPosition ].
	text text: self loremIpsum
]

{ #category : #private }
SpScrollSyncExample >> loremIpsum [
	^ String loremIpsum: 2854
]

{ #category : #accessing }
SpScrollSyncExample >> sideRuler [
	^ sideRuler
]

{ #category : #private }
SpScrollSyncExample >> syncMorphPosition [
	currentVerticalScrollPosition = text scrollValue y
		ifTrue: [ ^ self ].
	currentVerticalScrollPosition := text scrollValue y.
	self needRebuild: false.
	text needRebuild: false.
	self buildWithSpecLayout: self defaultSpecLayout

	
]

{ #category : #accessing }
SpScrollSyncExample >> text [
	^ text
]
