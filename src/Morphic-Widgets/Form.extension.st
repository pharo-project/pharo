Extension { #name : 'Form' }

{ #category : '*Morphic-Widgets' }
Form >> asAlphaImageMorph [

	^ AlphaImageMorph new image: self
]

{ #category : '*Morphic-Widgets' }
Form >> asMorph [
	^ImageMorph new form: self
]

{ #category : '*Morphic-Widgets' }
Form class >> floodFillTolerance [
	^ FloodFillTolerance ifNil: [FloodFillTolerance := 0.0]
]

{ #category : '*Morphic-Widgets' }
Form >> floodFillTolerance [
	^ self class floodFillTolerance
]

{ #category : '*Morphic-Widgets' }
Form >> listRenderOn: aCanvas atRow: index bounds: aRectangle color: aColor backgroundColor: backgroundColor from: aList [
	self asMorph
		listRenderOn: aCanvas
		atRow: index
		bounds: aRectangle
		color: aColor
		backgroundColor: backgroundColor
		from: aList
]

{ #category : '*Morphic-Widgets' }
Form class >> openImageInWindow: fullName [
	"Handle five file formats: GIF, JPG, PNG, Form storeOn: (run coded), and
	BMP. Fail if file format is not recognized."

	| image |
	image := self fromFileNamed: fullName.
	(ImageMorph withForm: image) openInWorld
]

{ #category : '*Morphic-Widgets' }
Form >> scaledByDisplayScaleFactor [

	^ self scaledToSize: self extent * WorldMorph displayScaleFactor
]

{ #category : '*Morphic-Widgets' }
Form >> scaledIntoFormOfSize: aNumberOrPoint [
	"Scale and center the receiver into a form of a given size"

	| extent scale scaledForm result |

	extent := aNumberOrPoint asPoint.
	extent = self extent ifTrue: [^ self].

	(self height isZero or: [self width isZero])
		ifTrue: [^ self class extent: extent depth: self depth].

	scale := extent y / self height min: extent x / self width.
	scaledForm := self
				magnifyBy: scale
				smoothing: 8.

	result := self class extent: extent depth: 32.
	result getCanvas
		translucentImage: scaledForm
		at: extent - scaledForm extent // 2.

	^ result
]

{ #category : '*Morphic-Widgets-Support' }
Form >> stencil [
	"return a 1-bit deep, black-and-white stencil of myself"

	| canvas |
	canvas := FormCanvas extent: self extent depth: 1.
	canvas fillColor: (Color white).

	canvas stencil: self at: 0@0
				sourceRect: (Rectangle origin: 0@0 corner: self extent) color: Color black.

	^ canvas form
]
