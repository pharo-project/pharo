"
BorderedMorph introduce borders to morph. Borders have the instanceVariables borderWidth and borderColor.
 
BorderedMorph new borderColor: Color red; borderWidth: 10; openInWorld.

BorderedMorph also have a varaity of border styles: simple, inset, raised, complexAltFramed, complexAltInset, complexAltRaised, complexFramed, complexInset, complexRaised.
These styles are set using the classes BorderStyle, SimpleBorder, RaisedBorder, InsetBorder and ComplexBorder.

BorderedMorph new borderStyle: (SimpleBorder width: 1 color: Color white); openInWorld.
BorderedMorph new borderStyle: (BorderStyle inset width: 2); openInWorld.



"
Class {
	#name : #BorderedMorph,
	#superclass : #Morph,
	#instVars : [
		'borderWidth',
		'borderColor'
	],
	#category : #'Morphic-Core-Kernel'
}

{ #category : #geometry }
BorderedMorph >> acquireBorderWidth: aBorderWidth [
	"Gracefully acquire the new border width, keeping the interior area intact and not seeming to shift"

	| delta |
	(delta := aBorderWidth- self borderWidth) = 0 ifTrue: [^ self].
	self bounds: ((self bounds origin - (delta @ delta)) corner: (self bounds corner + (delta @ delta))).
	self borderWidth: aBorderWidth.
	self layoutChanged
]

{ #category : #menu }
BorderedMorph >> addBorderStyleMenuItems: aMenu hand: aHandMorph [
	"Add border-style menu items"

	| subMenu |
	subMenu := UIManager default newMenuIn: self for: self.
	subMenu add: 'border width...' selector: #changeBorderWidth:.
	subMenu add: 'border color...' selector: #changeBorderColor:.
	subMenu addLine.
	BorderStyle borderStyleChoices do:
		[:sym | (self borderStyleForSymbol: sym)
			ifNotNil:
				[subMenu add: sym target: self selector: #setBorderStyle: argument: sym]].
	aMenu add: 'border style' subMenu: subMenu

]

{ #category : #private }
BorderedMorph >> basicBorderColor: aColor [ 
	borderColor := aColor
]

{ #category : #private }
BorderedMorph >> basicBorderWidth: aNumber [ 
	borderWidth := aNumber
]

{ #category : #accessing }
BorderedMorph >> borderColor [
	^ borderColor 
]

{ #category : #accessing }
BorderedMorph >> borderColor: colorOrSymbolOrNil [
	self doesBevels ifFalse:[
		colorOrSymbolOrNil isColor ifFalse:[^self]].
	borderColor = colorOrSymbolOrNil ifFalse: [
		borderColor := colorOrSymbolOrNil.
		self changed].

]

{ #category : #initialization }
BorderedMorph >> borderInitialize [
	"initialize the receiver state related to border"
	borderColor:= self defaultBorderColor.
	borderWidth := self defaultBorderWidth
]

{ #category : #accessing }
BorderedMorph >> borderInset [
	self borderColor: #inset
]

{ #category : #accessing }
BorderedMorph >> borderRaised [
	self borderColor: #raised
]

{ #category : #accessing }
BorderedMorph >> borderStyle [
	"Work around the borderWidth/borderColor pair"

	| style |
	borderColor ifNil: [^BorderStyle default].
	borderWidth isZero ifTrue: [^BorderStyle default].
	self assureExtension.
	style := extension borderStyle ifNil: [BorderStyle default].
	(borderWidth = style width and: 
			["Hah! Try understanding this..."

			borderColor == style style or: 
					["#raised/#inset etc"

					#simple == style style and: [borderColor = style color]]]) 
		ifFalse: 
			[style := borderColor isColor 
				ifTrue: [BorderStyle width: borderWidth color: borderColor]
				ifFalse: [(BorderStyle perform: borderColor) width: borderWidth	"argh."].
			extension borderStyle: style].
	^style trackColorFrom: self
]

{ #category : #accessing }
BorderedMorph >> borderStyle: aBorderStyle [ 
	"Work around the borderWidth/borderColor pair"

	aBorderStyle = self borderStyle ifTrue: [^self].
	self assureExtension.
	"secure against invalid border styles"
	(self canDrawBorder: aBorderStyle) 
		ifFalse: 
			["Replace the suggested border with a simple one"

			^self borderStyle: (BorderStyle width: aBorderStyle width
						color: (aBorderStyle trackColorFrom: self) color)].
	aBorderStyle width = self borderStyle width ifFalse: [self changed].
	(aBorderStyle isNil or: [aBorderStyle == BorderStyle default]) 
		ifTrue: 
			[extension borderStyle: nil.
			borderWidth := 0.
			^self changed].
	extension borderStyle: aBorderStyle.
	borderWidth := aBorderStyle width.
	borderColor := aBorderStyle style == #simple 
				ifTrue: [aBorderStyle color]
				ifFalse: [aBorderStyle style].
	self changed
]

{ #category : #accessing }
BorderedMorph >> borderWidth [
	^ borderWidth
]

{ #category : #accessing }
BorderedMorph >> borderWidth: anInteger [
	borderColor ifNil: [borderColor := Color black].
	borderWidth := anInteger max: 0.
	self changed
]

{ #category : #geometry }
BorderedMorph >> closestPointTo: aPoint [
	"account for round corners. Still has a couple of glitches at upper left and right corners"
	| pt |
	pt := self bounds pointNearestTo: aPoint.
	self wantsRoundedCorners ifFalse: [ ^pt ].
	self bounds corners with: (self bounds insetBy: 6) corners do: [ :out :in |
		(pt - out) abs < (6@6)
			ifTrue: [ ^(in + (Point r: 5.0 degrees: (pt - in) degrees)) asIntegerPoint ].
	].
	^pt.
]

{ #category : #accessing }
BorderedMorph >> colorForInsets [
	"Return the color to be used for shading inset borders."
	
	self owner isSystemWindow
		ifTrue: [^self owner colorForInsets].
	^super colorForInsets
]

{ #category : #initialization }
BorderedMorph >> defaultBorderColor [
	"answer the default border color/fill style for the receiver"
	^ Color black
]

{ #category : #initialization }
BorderedMorph >> defaultBorderWidth [
	"answer the default border width for the receiver"
	^ self theme borderWidth
]

{ #category : #accessing }
BorderedMorph >> doesBevels [
	"To return true means that this object can show bevelled borders, and
	therefore can accept, eg, #raised or #inset as valid borderColors.
	Must be overridden by subclasses that do not support bevelled borders."

	^ true
]

{ #category : #initialization }
BorderedMorph >> initialize [
	"initialize the state of the receiver"
	super initialize.
	
	self borderInitialize
]

{ #category : #testing }
BorderedMorph >> isTranslucentButNotTransparent [
	"Answer true if this any of this morph is translucent but not transparent."

	(color isColor and: [color isTranslucentButNotTransparent]) ifTrue: [^ true].
	(borderColor isColor and: [borderColor isTranslucentButNotTransparent]) ifTrue: [^ true].
	^ false

]

{ #category : #accessing }
BorderedMorph >> useSquareCorners [
	self cornerStyle: #square
]
