"
ImageMorph is a morph that displays a picture (Form). My extent is determined by the extent of my form.

Use #image: to set my picture.

Structure:
 instance var		Type 		Description
 formSet			FormSet	The FormSet to use when drawing

Code examples:
	ImageMorph new openInWorld; grabFromScreen

	(Form fromFileNamed: 'myGraphicsFileName') asMorph openInWorld

Relationship to SketchMorph: ImageMorph should be favored over SketchMorph, a parallel, legacy class -- see the Swiki FAQ for details ( http://minnow.cc.gatech.edu/squeak/1372 ). 

"
Class {
	#name : 'ImageMorph',
	#superclass : 'Morph',
	#traits : 'TAbleToRotate',
	#classTraits : 'TAbleToRotate classTrait',
	#instVars : [
		'formSet'
	],
	#classVars : [
		'DefaultForm'
	],
	#category : 'Morphic-Base-Basic',
	#package : 'Morphic-Base',
	#tag : 'Basic'
}

{ #category : 'accessing' }
ImageMorph class >> defaultForm [
	^DefaultForm
]

{ #category : 'instance creation' }
ImageMorph class >> fromStream: aStream [

	^self withForm: (ImageReadWriter formFromStream: aStream)
]

{ #category : 'instance creation' }
ImageMorph class >> fromString: aString [
	"Create a new ImageMorph which displays the input string in the standard button font"

	^ self fromString: aString font: StandardFonts buttonFont
]

{ #category : 'instance creation' }
ImageMorph class >> fromString: aString font: aFont [
	"Create a new ImageMorph showing the given string in the given font"

	^ self new form: (StringMorph contents: aString font: aFont) imageForm
]

{ #category : 'class initialization' }
ImageMorph class >> initialize [

	| h p d |
	DefaultForm := (Form extent: 80@40 depth: 16).
	h := DefaultForm height // 2.
	0 to: h - 1 do: [:i |
		p := (i * 2)@i.
		d := i asFloat / h asFloat.
		DefaultForm fill:
			(p corner: DefaultForm extent - p)
			fillColor: (Color r: d g: 0.5 b: 1.0 - d)]
]

{ #category : 'instance creation' }
ImageMorph class >> withForm: aForm [

	^ self withFormSet: (FormSet form: aForm)
]

{ #category : 'instance creation' }
ImageMorph class >> withFormSet: aFormSet [

	^ self new formSet: aFormSet; yourself
]

{ #category : 'menus' }
ImageMorph >> addCustomMenuItems: aMenu hand: aHand [
	super addCustomMenuItems: aMenu hand: aHand.
	aMenu addUpdating: #opacityString selector: #changeOpacity
]

{ #category : 'testing' }
ImageMorph >> areasRemainingToFill: aRectangle [

	^self isOpaque
		ifTrue: [aRectangle areasOutside: self bounds]
		ifFalse: [Array with: aRectangle]
]

{ #category : 'geometry' }
ImageMorph >> basicExtent: aPoint [

	"For sublcasses which need to use Morph>>#extent:"

	^super extent: aPoint
]

{ #category : 'accessing' }
ImageMorph >> borderStyle: newStyle [
	"Set the extent to include border width."

	| newExtent |
	self borderStyle = newStyle ifTrue: [^self].
	newExtent := 2 * newStyle width + formSet extent.
	bounds extent = newExtent ifFalse: [super extent: newExtent].
	super borderStyle: newStyle
]

{ #category : 'accessing' }
ImageMorph >> borderWidth: bw [

	| newExtent |
	newExtent := 2 * bw + formSet extent.
	bounds extent = newExtent ifFalse: [super extent: newExtent].
	super borderWidth: bw
]

{ #category : 'menu' }
ImageMorph >> changeOpacity [
	self isOpaque: self isOpaque not
]

{ #category : 'accessing' }
ImageMorph >> color: aColor [
	"Set the color.
	Change to a ColorForm here if depth 1."

	super color: aColor.
	(formSet depth = 1 and: [aColor isColor]) ifTrue: [
		formSet := FormSet extent: formSet extent depth: formSet depth
			forms: (formSet forms collect: [ :form |
				| colorForm |
				colorForm := form isColorForm ifTrue: [ form ] ifFalse: [ ColorForm mappingWhiteToTransparentFrom: (form asFormOfDepth: 1) ].
				colorForm colors: {Color transparent. aColor} ]) ].
	self changed
]

{ #category : 'initialization' }
ImageMorph >> defaultImage [
	"Answer the default image for the receiver."

	^ DefaultForm
]

{ #category : 'drawing' }
ImageMorph >> drawOn: aCanvas [
	"Draw the border after the image."

	| style |
	self isOpaque
		ifTrue: [aCanvas drawFormSet: formSet at: self innerBounds origin]
		ifFalse: [aCanvas translucentFormSet: formSet at: self innerBounds origin].
	(style := self borderStyle) ifNotNil: [style frameRectangle: bounds on: aCanvas]
]

{ #category : 'geometry' }
ImageMorph >> extent: aPoint [
	"Do nothing;
		My extent is determined by my image Form.
		For subclasses which need to set the extent, use #basicExtent:"
]

{ #category : 'accessing' }
ImageMorph >> form [
	^ formSet asForm
]

{ #category : 'accessing' }
ImageMorph >> form: aForm [

	self formSet: (FormSet form: aForm)
]

{ #category : 'accessing' }
ImageMorph >> formSet [

	^ formSet
]

{ #category : 'accessing' }
ImageMorph >> formSet: aFormSet [

	formSet := aFormSet.
	super extent: (2 * self borderWidth) asPoint + formSet extent.
	self changed
]

{ #category : 'menu commands' }
ImageMorph >> grabFromScreen [

	self form: Screenshot new formScreenshotFromUserSelection
]

{ #category : 'accessing' }
ImageMorph >> image: anImage [

	self form: anImage
]

{ #category : 'other' }
ImageMorph >> imageExport [

	^ self form bits asArray
]

{ #category : 'initialization' }
ImageMorph >> initialize [

	super initialize.
	self form: self defaultImage
]

{ #category : 'accessing' }
ImageMorph >> isOpaque [
	"Return true if the receiver is marked as being completely opaque"

	^ self
		valueOfProperty: #isOpaque
		ifAbsent: [false]
]

{ #category : 'accessing' }
ImageMorph >> isOpaque: aBool [
	"Mark the receiver as being completely opaque or not"

	aBool == false
		ifTrue: [self removeProperty: #isOpaque]
		ifFalse: [self setProperty: #isOpaque toValue: aBool].
	self changed
]

{ #category : 'menu' }
ImageMorph >> opacityString [
	^ (self isOpaque) -> 'opaque' translated
]

{ #category : 'menu commands' }
ImageMorph >> readFromFile [
	| fileName |
	fileName := self morphicUIManager
		request: 'Please enter the image file name' translated
		initialAnswer: 'fileName'.
	fileName isEmptyOrNil ifTrue: [^ self].
	self form: (Form fromFileNamed: fileName)
]

{ #category : 'caching' }
ImageMorph >> releaseCachedState [

	super releaseCachedState.
	formSet hibernate
]

{ #category : 'other' }
ImageMorph >> resize: newSize [

	| newExtent |

	newExtent := (newSize / formSet extent) min * formSet extent truncated.
	self formSet: (FormSet extent: newExtent depth: formSet depth
		forms: (formSet forms collect: [ :form | form scaledToSize: (form extent * (newExtent / formSet extent)) ]))
]

{ #category : 'testing' }
ImageMorph >> shouldFlex [
	^ true
]

{ #category : 'testing' }
ImageMorph >> wantsRecolorHandle [

	^ formSet isNotNil and: [formSet depth = 1]
]

{ #category : 'accessing' }
ImageMorph >> withSnapshotBorder [
	self
		borderStyle:
			((ComplexBorderStyle style: #complexFramed)
				color: (Color r: 0.613 g: 1.0 b: 0.516);
				width: 1;
				yourself)
]
