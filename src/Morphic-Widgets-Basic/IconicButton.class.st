"
A ""Simple Button"" in which the appearance is provided by a Form.
"
Class {
	#name : #IconicButton,
	#superclass : #SimpleButtonMorph,
	#instVars : [
		'graphicalMorph',
		'oldBorderStyle',
		'mouseOverBorderStyle'
	],
	#classVars : [
		'DefaultGraphics'
	],
	#category : #'Morphic-Widgets-Basic-Buttons'
}

{ #category : #initialize }
IconicButton class >> defaultGraphics [

	^ DefaultGraphics ifNil: [ DefaultGraphics := (Form extent: 20 @ 1 depth: 8)].
]

{ #category : #initialize }
IconicButton class >> initialize [
	"self initialize"
	
	DefaultGraphics := self defaultGraphics
]

{ #category : #menu }
IconicButton >> addLabelItemsTo: aCustomMenu hand: aHandMorph [
	"don't do the inherited behavior, since there is no textual label in this case"
]

{ #category : #accessing }
IconicButton >> borderInset [
	self borderStyle: (BorderStyle inset width: 2).
]

{ #category : #initialization }
IconicButton >> borderNormal [
	self borderStyle: (BorderStyle width: 2 color: Color transparent).
]

{ #category : #accessing }
IconicButton >> borderRaised [
	self borderStyle: (BorderStyle raised width: 2).
]

{ #category : #initialization }
IconicButton >> borderThick [
	self borderStyle: (BorderStyle width: 2 color: self raisedColor twiceDarker).
]

{ #category : #initialization }
IconicButton >> buttonSetup [
	self actWhen: #buttonUp.
	self cornerStyle: #rounded.
	self borderNormal.
	self on: #mouseEnter send: #borderRaised to: self.
	self on: #mouseLeave send: #borderNormal to: self.
	self on: #mouseLeaveDragging send: #borderNormal to: self.
	self on: #mouseDown send: #borderInset to: self.
	self on: #mouseUp send: #borderRaised to: self.
]

{ #category : #accessing }
IconicButton >> defaultGraphics [

	^  DefaultGraphics 
]

{ #category : #geometry }
IconicButton >> extent: newExtent [
	"Update the graphic position to keep centered."
	
	super extent: newExtent.
	graphicalMorph ifNotNil: [graphicalMorph position: self center - (graphicalMorph extent // 2)]
]

{ #category : #accessing }
IconicButton >> extraBorder [

	^ 6
]

{ #category : #accessing }
IconicButton >> graphicalMorph [

	^ graphicalMorph
]

{ #category : #accessing }
IconicButton >> graphicalMorph: aMorph [
	self addMorph: aMorph.
	aMorph lock.
]

{ #category : #'mouse events' }
IconicButton >> handlesMouseOver: anEvent [
	"Answer true, otherwise what is all that
	#mouseEnter:/#mouseLeave: stuff about?"

	^true
]

{ #category : #initialization }
IconicButton >> initialize [
	super initialize.
	self useSquareCorners
]

{ #category : #updating }
IconicButton >> interactSelector [

	 ^ #interact
]

{ #category : #accessing }
IconicButton >> labelFromString: aString [
	"Make an iconic label from aString"

	self labelGraphic: (StringMorph contents: aString) imageForm

]

{ #category : #accessing }
IconicButton >> labelGraphic [

	^ graphicalMorph form
]

{ #category : #accessing }
IconicButton >> labelGraphic: aForm [
	"Changed to look for any image morph rather than just a sketch."
	
	graphicalMorph ifNotNil: [graphicalMorph delete].
	graphicalMorph := ImageMorph new form: aForm.
	self extent: graphicalMorph extent + (self borderWidth + self extraBorder).
	graphicalMorph position: self center - (graphicalMorph extent // 2).
	self addMorph: graphicalMorph.
	graphicalMorph lock
]

{ #category : #'mouse events' }
IconicButton >> mouseDown: evt [
	"Partial workaraound for broken MouseOverHandler.
	Remove the border on mouse down if mouse focus has changed."

	super mouseDown: evt.
	(actWhen == #buttonDown and: [ evt hand mouseFocus ~= self ])
		ifTrue: [ self mouseLeave: evt ]
]

{ #category : #'mouse events' }
IconicButton >> mouseEnter: evt [
	"Remember the old border style."
	
	oldBorderStyle:= self borderStyle.
	self borderStyle: self mouseOverBorderStyle
]

{ #category : #'mouse events' }
IconicButton >> mouseLeave: evt [
	"Reinstate the old border style."

	oldBorderStyle ifNil: [^self borderNormal].
	self borderStyle: oldBorderStyle. 
	oldBorderStyle := nil.
		
]

{ #category : #'mouse events' }
IconicButton >> mouseOverBorderStyle [
	"Answer the border style to use whilst the mouse
	is over the receiver."

	^mouseOverBorderStyle ifNil: [BorderStyle thinGray]
]

{ #category : #'mouse events' }
IconicButton >> mouseOverBorderStyle: aBorderStyle [
	"Set the border style to use whilst the mouse
	is over the receiver."

	mouseOverBorderStyle := aBorderStyle.
]

{ #category : #initialization }
IconicButton >> setDefaultLabel [
	self labelGraphic: self class defaultGraphics
]

{ #category : #accessing }
IconicButton >> shedSelvedge [
	self extent: (self extent - (6@6))
]

{ #category : #initialization }
IconicButton >> stationarySetup [

	self actWhen: #startDrag.
	self cornerStyle: #rounded.
	self borderNormal.
	self on: #mouseEnter send: #borderThick to: self.
	self on: #mouseDown send: nil to: nil.
	self on: #mouseLeave send: #borderNormal to: self.
	self on: #mouseLeaveDragging send: #borderNormal to: self.
	self on: #mouseUp send: #borderThick to: self.
]

{ #category : #updating }
IconicButton >> themeChanged [

	"do nothing for now"
]

{ #category : #updating }
IconicButton >> update: aSelector [
	
	aSelector = self interactSelector
		ifTrue: [ ^ self doButtonAction ].
		
	super update: aSelector
]
