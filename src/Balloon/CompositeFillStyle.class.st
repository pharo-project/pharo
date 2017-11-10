"
Fillstyle supporting compositing of multiple sub-fillstyles.
"
Class {
	#name : #CompositeFillStyle,
	#superclass : #FillStyle,
	#instVars : [
		'fillStyles'
	],
	#category : #Balloon-Fills
}

{ #category : #build }
CompositeFillStyle class >> fillStyles: aCollection [
	"Answer a new instance of the receiver with the specfied fill styles."

	^self new fillStyles: aCollection
]

{ #category : #adding }
CompositeFillStyle >> addFillStyleMenuItems: aMenu hand: aHand from: aMorph [
	"Add the items for changing the current fill style of the receiver"
	aMenu add: 'change origin' target: self selector: #changeOriginIn:event: argument: aMorph.
	aMenu add: 'change orientation' target: self selector: #changeOrientationIn:event: argument: aMorph.
]

{ #category : #converting }
CompositeFillStyle >> asColor [
	"Answer a colour that is a best match to the receiver.
	Simple approach for the moment."
	
	^self fillStyles
		ifEmpty: [Color transparent]
		ifNotEmpty: [self fillStyles last asColor]
]

{ #category : #action }
CompositeFillStyle >> changeOrientationIn: aMorph event: evt [
	"Interactively change the origin of the receiver"
	| handle |
	handle := HandleMorph new forEachPointDo:[:pt|
		self direction: pt - self origin.
		self normal: nil.
		aMorph changed].
	evt hand attachMorph: handle.
	handle startStepping.
]

{ #category : #action }
CompositeFillStyle >> changeOriginIn: aMorph event: evt [
	"Interactively change the origin of the receiver"
	| handle |
	handle := HandleMorph new forEachPointDo:[:pt|
		self origin: pt.
		aMorph changed].
	evt hand attachMorph: handle.
	handle startStepping.
]

{ #category : #accessing }
CompositeFillStyle >> direction [
	"Answer an effective direction of any oriented fill styles.
	Answer the bottom-right maxima."

	|dir|
	dir := nil.
	self fillStyles reverseDo: [:fs |
		fs isOrientedFill ifTrue: [
			dir := dir
				ifNil: [fs direction]
				ifNotNil: [dir max: fs direction]]].
	^dir ifNil: [0@0] "just in case"
]

{ #category : #accessing }
CompositeFillStyle >> direction: aPoint [
	"Change the effective direction of any oriented fill styles."

	|delta|
	delta := aPoint - self direction.
	self fillStyles reverseDo: [:fs |
		fs isOrientedFill ifTrue: [
			fs direction: fs direction + delta]]
]

{ #category : #action }
CompositeFillStyle >> fillRectangle: aRectangle on: aCanvas [
	"Fill the given rectangle on the given canvas with the receiver.
	Render from bottom to top."
	
	self fillStyles do: [:fs |
		fs fillRectangle: aRectangle on: aCanvas]
]

{ #category : #accessing }
CompositeFillStyle >> fillStyles [
	"Answer the value of fillStyles. The first item in the collection is considered
	to be topmost when rendered."

	^ fillStyles
]

{ #category : #accessing }
CompositeFillStyle >> fillStyles: aCollection [
	"Set the value of fillStyles. The first item in the collection is considered
	to be topmost when rendering."

	fillStyles := aCollection
]

{ #category : #initialization }
CompositeFillStyle >> initialize [
	"Initialize the receiver."

	super initialize.
	self
		fillStyles: OrderedCollection new
]

{ #category : #testing }
CompositeFillStyle >> isCompositeFill [
	"Answer whether the receiver is a composite fill.
	True for kinds of the receiver's class."

	^true
]

{ #category : #testing }
CompositeFillStyle >> isGradientFill [
	"Answer whether any of the composited fill styles are gradients."

	self fillStyles reverseDo: [:fs |
		fs isGradientFill ifTrue: [^true]].
	^false
]

{ #category : #testing }
CompositeFillStyle >> isOrientedFill [
	"Answer whether any of the composited fill styles are oriented."

	self fillStyles reverseDo: [:fs |
		fs isOrientedFill ifTrue: [^true]].
	^false
]

{ #category : #testing }
CompositeFillStyle >> isTranslucent [
	"Answer whether all of the composited fill styles are transparent."

	^self fillStyles allSatisfy: [:fs | fs isTranslucent]
]

{ #category : #testing }
CompositeFillStyle >> isTransparent [
	"Answer whether all of the composited fill styles are transparent."

	^self fillStyles allSatisfy: [:fs | fs isTransparent]
]

{ #category : #accessing }
CompositeFillStyle >> normal [
	"Answer an effective normal of any oriented fill styles.
	Answer the top-left minima (probably not an accurate assumption)."

	|normal|
	normal := nil.
	self fillStyles reverseDo: [:fs |
		fs isOrientedFill ifTrue: [
			normal := normal
				ifNil: [fs normal]
				ifNotNil: [normal min: fs normal]]].
	^normal ifNil: [0@0] "just in case"
]

{ #category : #accessing }
CompositeFillStyle >> normal: aPoint [
	"Change the effective normal of any oriented fill styles."

	|delta|
	aPoint ifNil: [
		self fillStyles reverseDo: [:fs |
		fs isOrientedFill ifTrue: [
			fs normal: nil]].
		^self].
	delta := aPoint - self normal.
	self fillStyles reverseDo: [:fs |
		fs isOrientedFill ifTrue: [
			fs normal: fs normal + delta]]
]

{ #category : #accessing }
CompositeFillStyle >> origin [
	"Answer an effective origin of any oriented fill styles.
	Answer the top-left minima."

	|origin|
	origin := nil.
	self fillStyles reverseDo: [:fs |
		fs isOrientedFill ifTrue: [
			origin := origin
				ifNil: [fs origin]
				ifNotNil: [origin min: fs origin]]].
	^origin ifNil: [0@0] "just in case"
]

{ #category : #accessing }
CompositeFillStyle >> origin: aPoint [
	"Change the effective origin of any oriented fill styles."

	|delta|
	delta := aPoint - self origin.
	self fillStyles reverseDo: [:fs |
		fs isOrientedFill ifTrue: [
			fs origin: fs origin + delta]]
	
]
