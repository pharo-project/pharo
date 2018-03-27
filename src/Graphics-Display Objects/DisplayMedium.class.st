"
I am a display object which can both paint myself on a medium (displayOn: messages), and can act as a medium myself. My chief subclass is Form.
"
Class {
	#name : #DisplayMedium,
	#superclass : #DisplayObject,
	#classVars : [
		'HighLightBitmaps'
	],
	#category : #'Graphics-Display Objects'
}

{ #category : #bordering }
DisplayMedium >> border: aRectangle margins: insets rule: combinationRule fillColor: aHalfTone [
	"Paint a border whose rectangular area is defined by aRectangle. The 
	width of each edge of the border is determined by the four coordinates 
	of insets. Uses aHalfTone and combinationRule for drawing the border."

	(aRectangle areasOutside: (insets insetRectangle: aRectangle )) do:
		[:edgeStrip | self fill: edgeStrip rule: combinationRule fillColor: aHalfTone]
]

{ #category : #bordering }
DisplayMedium >> border: aRectangle width: borderWidth [ 
	"Paint a border whose rectangular area is defined by aRectangle. The 
	width of the border of each side is borderWidth. Uses theme borderColor for 
	drawing the border."

	self 
		border: aRectangle 
		width: borderWidth 
		fillColor: Smalltalk ui theme borderColor.

]

{ #category : #bordering }
DisplayMedium >> border: aRectangle width: borderWidth fillColor: aHalfTone [ 
	"Paint a border whose rectangular area is defined by aRectangle. The width of the border of each side is borderWidth. Uses aHalfTone for drawing the border."

	self border: aRectangle
		width: borderWidth
		rule: Form over
		fillColor: aHalfTone
]

{ #category : #bordering }
DisplayMedium >> border: aRectangle width: borderWidth rule: combinationRule fillColor: aHalfTone [ 
	"Paint a border whose rectangular area is defined by aRectangle. The width of the border of each side is borderWidth. Uses aHalfTone for drawing the border."

	self border: aRectangle
		margins: 
			(Margin
				left: borderWidth
				right: borderWidth
				top: borderWidth
				bottom: borderWidth)
		rule: combinationRule
		fillColor: aHalfTone
]

{ #category : #bordering }
DisplayMedium >> border: aRectangle widthRectangle: insets rule: combinationRule fillColor: aHalfTone [
	"Paint a border whose rectangular area is defined by aRectangle. The 
	width of each edge of the border is determined by the four coordinates 
	of insets. Uses aHalfTone and combinationRule for drawing the border."

	| deprecationMessage |
	deprecationMessage := 'Use #border:margins:rule:fillColor: instead'.
	insets class = Rectangle
		ifTrue:
			[ deprecationMessage := deprecationMessage
				,
					'\Change sender to use Margin instead of Rectangle. \ Rectangles are now normalized, and can no longer interchangeably be used as margins'
						withCRs ].
	self deprecated: deprecationMessage on: '10 December 2017' in: 'Pharo70'.
	^ self
		border: aRectangle
		margins: insets asMargin
		rule: combinationRule
		fillColor: aHalfTone
]

{ #category : #displaying }
DisplayMedium >> copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: clipRect rule: rule fillColor: aForm [ 
	"Make up a BitBlt table and copy the bits."

	self subclassResponsibility
]

{ #category : #displaying }
DisplayMedium >> deferUpdatesIn: aRectangle while: aBlock [
	"DisplayScreen overrides with something more involved..."
	^aBlock value
]

{ #category : #displaying }
DisplayMedium >> drawLine: sourceForm from: beginPoint to: endPoint clippingBox: clipRect rule: anInteger fillColor: aForm [ 
	"Draw line by copying the argument, sourceForm, starting at location 
	beginPoint and ending at endPoint, clipped by the rectangle, clipRect. 
	The rule and mask for copying are the arguments anInteger and aForm."

	self subclassResponsibility
]

{ #category : #coloring }
DisplayMedium >> fill: aRectangle fillColor: aForm [ 
	"Replace a rectangular area of the receiver with the pattern described by 
	aForm according to the rule over."

	self fill: aRectangle rule: Form over fillColor: aForm
]

{ #category : #coloring }
DisplayMedium >> fill: aRectangle rule: anInteger fillColor: aForm [ 
	"Replace a rectangular area of the receiver with the pattern described by 
	aForm according to the rule anInteger."

	self subclassResponsibility
]

{ #category : #coloring }
DisplayMedium >> fillBlack [
	"Set all bits in the receiver to black (ones)."

	self fill: self boundingBox fillColor: Color black
]

{ #category : #coloring }
DisplayMedium >> fillBlack: aRectangle [ 
	"Set all bits in the receiver's area defined by aRectangle to black (ones)."

	self fill: aRectangle rule: Form over fillColor: Color black
]

{ #category : #coloring }
DisplayMedium >> fillColor: aColor [
	"Set all pixels in the receiver to the color.  Must be a correct color for this depth of medium.  TK 1 Jun 96"

	self fill: self boundingBox fillColor: aColor
]

{ #category : #coloring }
DisplayMedium >> fillGray [
	"Set all bits in the receiver to gray."

	self fill: self boundingBox fillColor: Color gray
]

{ #category : #coloring }
DisplayMedium >> fillGray: aRectangle [
	"Set all bits in the receiver's area defined by aRectangle to the gray mask."

	self fill: aRectangle rule: Form over fillColor: Color gray
]

{ #category : #coloring }
DisplayMedium >> fillShape: aShapeForm fillColor: aColor [
	"Fill a region corresponding to 1 bits in aShapeForm with aColor"

	^ self fillShape: aShapeForm fillColor: aColor at: 0@0
]

{ #category : #coloring }
DisplayMedium >> fillShape: aShapeForm fillColor: aColor at: location [
	"Fill a region corresponding to 1 bits in aShapeForm with aColor"

	((BitBlt destForm: self sourceForm: aShapeForm fillColor: aColor
		combinationRule: Form paint
		destOrigin: location + aShapeForm offset sourceOrigin: 0@0
		extent: self extent clipRect: self boundingBox)
		colorMap: (Bitmap with: 0 with: 16rFFFFFFFF))
		copyBits
]

{ #category : #coloring }
DisplayMedium >> fillWhite [
	"Set all bits in the form to white."

	self fill: self boundingBox fillColor: Color white.

]

{ #category : #coloring }
DisplayMedium >> fillWhite: aRectangle [
	"Set all bits in the receiver's area defined by aRectangle to white."

	self fill: aRectangle rule: Form over fillColor: Color white.

]

{ #category : #coloring }
DisplayMedium >> fillWithColor: aColor [
	"Fill the receiver's bounding box with the given color."

	self fill: self boundingBox fillColor: aColor.

]

{ #category : #private }
DisplayMedium >> highLightBitmaps [
	"Quickly return a Bitblt-ready raw colorValue for highlighting areas."
	^HighLightBitmaps ifNil: [self initializeHighLights ].

]

{ #category : #private }
DisplayMedium >> initializeHighLights [ 
	
	"Create a set of Bitmaps for quickly reversing areas of the screen without converting colors. "
	
	HighLightBitmaps := Array new: 32.
	^HighLightBitmaps
		at: 1 put: (Bitmap with: 4294967295);
		at: 2 put: (Bitmap with: 4294967295);
		at: 4 put: (Bitmap with: 1431655765);
		at: 8 put: (Bitmap with: 117901063);
		at: 16 put: (Bitmap with: 4294967295);
		at: 32 put: (Bitmap with: 4294967295);
		yourself.

]

{ #category : #private }
DisplayMedium >> quickHighLight: aDepth [
	^ self highLightBitmaps at: aDepth
]

{ #category : #coloring }
DisplayMedium >> reverse [
	"Change all the bits in the receiver that are white to black, and the ones 
	that are black to white."

"
Display reverse: (10@10 extent: 100@100)
"

	self fill: self boundingBox rule: Form reverse fillColor: (self quickHighLight: self depth)
]

{ #category : #coloring }
DisplayMedium >> reverse: aRectangle [
	"Change all the bits in the receiver's area that intersects with aRectangle 
	that are white to black, and the ones that are black to white."

	self fill: aRectangle rule: Form reverse fillColor: (self quickHighLight: self depth)
]

{ #category : #coloring }
DisplayMedium >> reverse: aRectangle fillColor: aMask [	
	"Change all the bits in the receiver's area that intersects with aRectangle 
	according to the mask. Black does not necessarily turn to white, rather it 
	changes with respect to the rule and the bit in a corresponding mask 
	location. Bound to give a surprise."

	self fill: aRectangle rule: Form reverse fillColor: aMask
]
