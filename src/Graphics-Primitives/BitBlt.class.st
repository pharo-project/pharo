"
I represent a block transfer (BLT) of pixels into a rectangle (destX, destY, width, height) of the destinationForm.  The source of pixels may be a similar rectangle (at sourceX, sourceY) in the sourceForm, or a constant color, currently called halftoneForm.  If both are specified, their pixel values are combined with a logical AND function prior to transfer.  In any case, the pixels from the source are combined with those of the destination by as specified by the combinationRule.

The combination rule whose value is 0 through 15 programs the transfer to produce 1 or 0 according to its 4-bit representation as follows:
	8:	if source is 0 and destination is 0
	4:	if source is 0 and destination is 1
	2:	if source is 1 and destination is 0
	1:	if source is 1 and destination is 1.
At each pixel the corresponding bits of the source and destination pixel values determine one of these conditions;  if the combination rule has a 1 in the corresponding bit position, then the new destination value will be 1, otherwise it will be zero.  Forms may be of different depths, see the comment in class Form.

In addition to the original 16 combination rules, this BitBlt supports
	16	fails (to simulate paint bits)
	17	fails (to simulate erase bits)
	18	sourceWord + destinationWord
	19	sourceWord - destinationWord
	20	rgbAdd: sourceWord with: destinationWord.  Sum of color components
	21	rgbSub: sourceWord with: destinationWord.  Difference of color components
	22	OLDrgbDiff: sourceWord with: destinationWord.  Sum of abs of differences in components
	23	OLDtallyIntoMap: destinationWord.  Tallies pixValues into a colorMap
			these old versions don't do bitwise dest clipping.  Use 32 and 33 now.
	24	alphaBlend: sourceWord with: destinationWord.  32-bit source and dest only
	25	pixPaint: sourceWord with: destinationWord.  Wherever the sourceForm is non-zero, it replaces the destination.  Can be used with a 1-bit source color mapped to (0, FFFFFFFF), and a fillColor to fill the dest with that color wherever the source is 1.
	26	pixMask: sourceWord with: destinationWord.  Like pixPaint, but fills with 0.
	27	rgbMax: sourceWord with: destinationWord.  Max of each color component.
	28	rgbMin: sourceWord with: destinationWord.  Min of each color component.
	29	rgbMin: sourceWord bitInvert32 with: destinationWord.  Min with (max-source)
	30	alphaBlendConst: sourceWord with: destinationWord.  alpha is an arg. works in 16 bits.
	31	alphaPaintConst: sourceWord with: destinationWord.  alpha is an arg. works in 16 bits.
	32	rgbDiff: sourceWord with: destinationWord.  Sum of abs of differences in components
	33	tallyIntoMap: destinationWord.  Tallies pixValues into a colorMap
	34	alphaBlendScaled: srcWord with: dstWord. Alpha blend of scaled srcWord and destWord.

The color specified by halftoneForm may be either a Color or a Pattern.   A Color is converted to a pixelValue for the depth of the destinationForm.  If a Pattern, BitBlt will simply interpret its bitmap as an array of Color pixelValues.  BitBlt aligns the first element of this array with the top scanline of the destinationForm, the second with the second, and so on, cycling through the color array as necessary.  Within each scan line the 32-bit value is repeated from left to right across the form.  If the value repeats on pixels boudaries, the effect will be a constant color;  if not, it will produce a halftone that repeats on 32-bit boundaries.

Any transfer specified is further clipped by the specified rectangle (clipX, clipY, clipWidth, clipHeight), and also by the bounds of the source and destination forms.
	To make a small Form repeat and fill a big form, use an InfiniteForm as the source.
	To write on a form and leave with both transparent and opapue areas, use a MaskedForm as the source.

Pixels from a source to a destination whose pixels have a different depth are converted based on the optional colorMap.  If colorMap is nil, then conversion to more bits is done by filling the new high-order bits with zero, and conversion to fewer bits is done by truncating the lost high-order bits.  

The colorMap, if specified, must be a either word array (ie Bitmap) with 2^n elements, where n is the pixel depth of the source, or a fully specified ColorMap which may contain a lookup table (ie Bitmap) and/or four separate masks and shifts which are applied to the pixels. For every source pixel, BitBlt will first perform masking and shifting and then index the lookup table, and select the corresponding pixelValue and mask it to the destination pixel size before storing.
	When blitting from a 32 or 16 bit deep Form to one 8 bits or less, the default is truncation.  This will produce very strange colors, since truncation of the high bits does not produce the nearest encoded color.  Supply a 512 long colorMap, and red, green, and blue will be shifted down to 3 bits each, and mapped.  The message copybits...stdColors will use the best map to the standard colors for destinations of depths 8, 4, 2 and 1.  Two other sized of colorMaps are allowed, 4096 (4 bits per color) and 32786 (five bits per color).
	Normal blits between 16 and 32 bit forms truncates or pads the colors automatically to provide the best preservation of colors.
	Colors can be remapped at the same depth.  Sometimes a Form is in terms of colors that are not the standard colors for this depth, for example in a GIF file.  Convert the Form to a MaskedForm and send colorMap: the list of colors that the picture is in terms of.  MaskedForm will use the colorMap when copying to the display or another Form. (Note also that a Form can be copied to itself, and transformed in the process, if a non-nil colorMap is supplied.)
"
Class {
	#name : #BitBlt,
	#superclass : #Object,
	#instVars : [
		'destForm',
		'sourceForm',
		'halftoneForm',
		'combinationRule',
		'destX',
		'destY',
		'width',
		'height',
		'sourceX',
		'sourceY',
		'clipX',
		'clipY',
		'clipWidth',
		'clipHeight',
		'colorMap'
	],
	#classVars : [
		'CachedFontColorMaps',
		'ColorConvertingMaps'
	],
	#category : 'Graphics-Primitives'
}

{ #category : #'instance creation' }
BitBlt class >> bitPeekerFromForm: sourceForm [ 
	"Answer an instance to be used extract individual pixels from the given Form. The destination for a 1x1 copyBits will be the low order bits of (bits at: 1)."
	| pixPerWord |
	pixPerWord := 32 // sourceForm depth.
	sourceForm unhibernate.
	^ self 
		destForm: (Form 
				extent: pixPerWord @ 1
				depth: sourceForm depth)
		sourceForm: sourceForm
		halftoneForm: nil
		combinationRule: Form over
		destOrigin: (pixPerWord - 1) @ 0
		sourceOrigin: 0 @ 0
		extent: 1 @ 1
		clipRect: (0 @ 0 extent: pixPerWord @ 1)
]

{ #category : #'instance creation' }
BitBlt class >> bitPokerToForm: destForm [ 
	"Answer an instance to be used for valueAt: aPoint put: pixValue.
	The source for a 1x1 copyBits will be the low order of (bits at: 1)"
	| pixPerWord |
	pixPerWord := 32 // destForm depth.
	destForm unhibernate.
	^ self 
		destForm: destForm
		sourceForm: (Form 
				extent: pixPerWord @ 1
				depth: destForm depth)
		halftoneForm: nil
		combinationRule: Form over
		destOrigin: 0 @ 0
		sourceOrigin: (pixPerWord - 1) @ 0
		extent: 1 @ 1
		clipRect: (0 @ 0 extent: destForm extent)
]

{ #category : #cleanup }
BitBlt class >> cleanUp [
	"Flush caches"

	self recreateColorMaps.
]

{ #category : #'instance creation' }
BitBlt class >> destForm: df sourceForm: sf fillColor: hf combinationRule: cr destOrigin: destOrigin sourceOrigin: sourceOrigin extent: extent clipRect: clipRect [ 
	"Answer an instance of me with values set according to the arguments."

	^ self new
		setDestForm: df
		sourceForm: sf
		fillColor: hf
		combinationRule: cr
		destOrigin: destOrigin
		sourceOrigin: sourceOrigin
		extent: extent
		clipRect: clipRect
]

{ #category : #'instance creation' }
BitBlt class >> destForm: df sourceForm: sf halftoneForm: hf combinationRule: cr destOrigin: destOrigin sourceOrigin: sourceOrigin extent: extent clipRect: clipRect [ 
	"Answer an instance of me with values set according to the arguments."

	^ self new
		setDestForm: df
		sourceForm: sf
		fillColor: hf
		combinationRule: cr
		destOrigin: destOrigin
		sourceOrigin: sourceOrigin
		extent: extent
		clipRect: clipRect
]

{ #category : #'class initialization' }
BitBlt class >> initialize [
	self recreateColorMaps
]

{ #category : #private }
BitBlt class >> recreateColorMaps [
	CachedFontColorMaps := ColorConvertingMaps := nil
]

{ #category : #'instance creation' }
BitBlt class >> toForm: aForm [
	^ self new setDestForm: aForm
]

{ #category : #accessing }
BitBlt >> clipBy: aRectangle [ 
	| aPoint right bottom |
	right := clipX + clipWidth.
	bottom := clipY + clipHeight.
	aPoint := aRectangle origin.
	aPoint x > clipX ifTrue: [ clipX := aPoint x ].
	aPoint y > clipY ifTrue: [ clipY := aPoint y ].
	aPoint := aRectangle corner.
	aPoint x < right ifTrue: [ right := aPoint x ].
	aPoint y < bottom ifTrue: [ bottom := aPoint y ].
	clipWidth := right - clipX.
	clipHeight := bottom - clipY.
	clipWidth < 0 ifTrue: [ clipWidth := 0 ].
	clipHeight < 0 ifTrue: [ clipHeight := 0 ]
]

{ #category : #accessing }
BitBlt >> clipByX1: x1 y1: y1 x2: x2 y2: y2 [ 
	| right bottom |
	right := clipX + clipWidth.
	bottom := clipY + clipHeight.
	x1 > clipX ifTrue: [ clipX := x1 ].
	y1 > clipY ifTrue: [ clipY := y1 ].
	x2 < right ifTrue: [ right := x2 ].
	y2 < bottom ifTrue: [ bottom := y2 ].
	clipWidth := right - clipX.
	clipHeight := bottom - clipY.
	clipWidth < 0 ifTrue: [ clipWidth := 0 ].
	clipHeight < 0 ifTrue: [ clipHeight := 0 ]
]

{ #category : #accessing }
BitBlt >> clipHeight [
	^clipHeight
]

{ #category : #accessing }
BitBlt >> clipHeight: anInteger [ 
	"Set the receiver's clipping area height to be the argument, anInteger."
	clipHeight := anInteger
]

{ #category : #private }
BitBlt >> clipRange [
	"clip and adjust source origin and extent appropriately"
	"first in x"
	"fill in the lazy state if needed"
	| sx sy dx dy bbW bbH |
	destX ifNil: [ destX := 0 ].
	destY ifNil: [ destY := 0 ].
	width ifNil: [ width := destForm width ].
	height ifNil: [ height := destForm height ].
	sourceX ifNil: [ sourceX := 0 ].
	sourceY ifNil: [ sourceY := 0 ].
	clipX ifNil: [ clipX := 0 ].
	clipY ifNil: [ clipY := 0 ].
	clipWidth ifNil: [ clipWidth := destForm width ].
	clipHeight ifNil: [ clipHeight := destForm height ].
	destX >= clipX 
		ifTrue: 
			[ sx := sourceX.
			dx := destX.
			bbW := width ]
		ifFalse: 
			[ sx := sourceX + (clipX - destX).
			bbW := width - (clipX - destX).
			dx := clipX ].
	dx + bbW > (clipX + clipWidth) ifTrue: [ bbW := bbW - (dx + bbW - (clipX + clipWidth)) ].
	"then in y"
	destY >= clipY 
		ifTrue: 
			[ sy := sourceY.
			dy := destY.
			bbH := height ]
		ifFalse: 
			[ sy := sourceY + clipY - destY.
			bbH := height - (clipY - destY).
			dy := clipY ].
	dy + bbH > (clipY + clipHeight) ifTrue: [ bbH := bbH - (dy + bbH - (clipY + clipHeight)) ].
	sourceForm ifNotNil: 
		[ sx < 0 ifTrue: 
			[ dx := dx - sx.
			bbW := bbW + sx.
			sx := 0 ].
		sx + bbW > sourceForm width ifTrue: [ bbW := bbW - (sx + bbW - sourceForm width) ].
		sy < 0 ifTrue: 
			[ dy := dy - sy.
			bbH := bbH + sy.
			sy := 0 ].
		sy + bbH > sourceForm height ifTrue: [ bbH := bbH - (sy + bbH - sourceForm height) ] ].
	(bbW <= 0 or: [ bbH <= 0 ]) ifTrue: 
		[ sourceX := sourceY := destX := destY := clipX := clipY := width := height := 0.
		^ true ].
	(sx = sourceX and: 
		[ sy = sourceY and: 
			[ dx = destX and: [ dy = destY and: [ bbW = width and: [ bbH = height ] ] ] ] ]) ifTrue: [ ^ false ].
	sourceX := sx.
	sourceY := sy.
	destX := dx.
	destY := dy.
	width := bbW.
	height := bbH.
	^ true
]

{ #category : #accessing }
BitBlt >> clipRect [
	"Answer the receiver's clipping area rectangle."

	^clipX @ clipY extent: clipWidth @ clipHeight
]

{ #category : #accessing }
BitBlt >> clipRect: aRectangle [ 
	"Set the receiver's clipping area rectangle to be the argument, aRectangle."
	clipX := aRectangle left truncated.
	clipY := aRectangle top truncated.
	clipWidth := aRectangle right truncated - clipX.
	clipHeight := aRectangle bottom truncated - clipY
]

{ #category : #accessing }
BitBlt >> clipWidth [
	^clipWidth
]

{ #category : #accessing }
BitBlt >> clipWidth: anInteger [ 
	"Set the receiver's clipping area width to be the argument, anInteger."
	clipWidth := anInteger
]

{ #category : #accessing }
BitBlt >> clipX [
	^clipX
]

{ #category : #accessing }
BitBlt >> clipX: anInteger [ 
	"Set the receiver's clipping area top left x coordinate to be the argument, 
	anInteger."
	clipX := anInteger
]

{ #category : #accessing }
BitBlt >> clipY [
	^clipY
]

{ #category : #accessing }
BitBlt >> clipY: anInteger [ 
	"Set the receiver's clipping area top left y coordinate to be the argument, 
	anInteger."
	clipY := anInteger
]

{ #category : #accessing }
BitBlt >> color [
	"Return the current fill color as a Color.  
	 Gives the wrong answer if the halftoneForm is a complex pattern of more than one word."

	halftoneForm ifNil: [^ Color black].
	^ Color colorFromPixelValue: halftoneForm first depth: destForm depth
]

{ #category : #private }
BitBlt >> colorConvertingMap: targetColor from: sourceDepth to: destDepth keepSubPixelAA: keepSubPix [ 
	| srcIndex map mapsForSource mapsForSourceAndDest |
	ColorConvertingMaps class == Array ifFalse: [ ColorConvertingMaps := (1 to: 10) collect: [ :i | Array new: 32 ] ].
	srcIndex := sourceDepth.
	sourceDepth > 8 ifTrue: 
		[ srcIndex := keepSubPix 
			ifTrue: [ 9 ]
			ifFalse: [ 10 ] ].
	mapsForSource := ColorConvertingMaps at: srcIndex.
	(mapsForSourceAndDest := mapsForSource at: destDepth) ifNil: 
		[ mapsForSourceAndDest := mapsForSource 
			at: destDepth
			put: Dictionary new ].
	map := mapsForSourceAndDest 
		at: targetColor
		ifAbsentPut: 
			[ Color 
				computeColorConvertingMap: targetColor
				from: sourceDepth
				to: destDepth
				keepSubPixelAA: keepSubPix ].
	^ map
]

{ #category : #accessing }
BitBlt >> colorMap [
	^ colorMap
]

{ #category : #accessing }
BitBlt >> colorMap: map [ 
	"See last part of BitBlt comment."
	colorMap := map
]

{ #category : #accessing }
BitBlt >> combinationRule: anInteger [ 
	"Set the receiver's combination rule to be the argument, anInteger, a 
	number in the range 0-15."
	combinationRule := anInteger
]

{ #category : #copying }
BitBlt >> copy: destRectangle from: sourcePt in: srcForm [ 
	| destOrigin |
	sourceForm := srcForm.
	halftoneForm := nil.
	combinationRule := 3.	"store"
	destOrigin := destRectangle origin.
	destX := destOrigin x.
	destY := destOrigin y.
	sourceX := sourcePt x.
	sourceY := sourcePt y.
	width := destRectangle width.
	height := destRectangle height.
	self copyBits
]

{ #category : #copying }
BitBlt >> copy: destRectangle from: sourcePt in: srcForm fillColor: hf rule: rule [ 
	"Specify a Color to fill, not a Form."
	| destOrigin |
	sourceForm := srcForm.
	self fillColor: hf.	"sets halftoneForm"
	combinationRule := rule.
	destOrigin := destRectangle origin.
	destX := destOrigin x.
	destY := destOrigin y.
	sourceX := sourcePt x.
	sourceY := sourcePt y.
	width := destRectangle width.
	height := destRectangle height.
	srcForm == nil ifFalse: [ colorMap := srcForm colormapIfNeededFor: destForm ].
	^ self copyBits
]

{ #category : #copying }
BitBlt >> copy: destRectangle from: sourcePt in: srcForm halftoneForm: hf rule: rule [ 
	| destOrigin |
	sourceForm := srcForm.
	self fillColor: hf.	"sets halftoneForm"
	combinationRule := rule.
	destOrigin := destRectangle origin.
	destX := destOrigin x.
	destY := destOrigin y.
	sourceX := sourcePt x.
	sourceY := sourcePt y.
	width := destRectangle width.
	height := destRectangle height.
	self copyBits
]

{ #category : #copying }
BitBlt >> copyBits [
	"Primitive. Perform the movement of bits from the source form to the 
	destination form. Fail if any variables are not of the right type (Integer, 
	Float, or Form) or if the combination rule is not implemented. 
	In addition to the original 16 combination rules, this BitBlt supports
	16	fail (to simulate paint)
	17	fail (to simulate mask)
	18	sourceWord + destinationWord
	19	sourceWord - destinationWord
	20	rgbAdd: sourceWord with: destinationWord
	21	rgbSub: sourceWord with: destinationWord
	22	rgbDiff: sourceWord with: destinationWord
	23	tallyIntoMap: destinationWord
	24	alphaBlend: sourceWord with: destinationWord
	25	pixPaint: sourceWord with: destinationWord
	26	pixMask: sourceWord with: destinationWord
	27	rgbMax: sourceWord with: destinationWord
	28	rgbMin: sourceWord with: destinationWord
	29	rgbMin: sourceWord bitInvert32 with: destinationWord
"
	"Check for compressed source, destination or halftone forms"
	<primitive: 'primitiveCopyBits' module: 'BitBltPlugin' error: ec>
	ec == #'object moved' ifTrue:
		[ ^self copyBits ].
	(combinationRule between: 30 and: 31) ifTrue: 
		[ "No alpha specified -- re-run with alpha = 1.0"
		^ self copyBitsTranslucent: 255 ].
	(sourceForm isForm and: [ sourceForm unhibernate ]) ifTrue: [ ^ self copyBits ].
	(destForm isForm and: [ destForm unhibernate ]) ifTrue: [ ^ self copyBits ].
	(halftoneForm isForm and: [ halftoneForm unhibernate ]) ifTrue: [ ^ self copyBits ].

	"Check for unimplmented rules"
	combinationRule = Form oldPaint ifTrue: [ ^ self paintBits ].
	combinationRule = Form oldErase1bitShape ifTrue: [ ^ self eraseBits ].

	"Check if BitBlt doesn't support full color maps"
	(colorMap notNil and: [ colorMap isColormap ]) ifTrue: 
		[ colorMap := colorMap colors.
		^ self copyBits ].
	"Check if clipping gots us way out of range"
	self clipRange ifTrue: 
		[ self roundVariables.
		^ self copyBitsAgain ].
	self error: 'Bad BitBlt arg (Fraction?); proceed to convert.'.
	"Convert all numeric parameters to integers and try again."
	self roundVariables.
	^ self copyBitsAgain
]

{ #category : #private }
BitBlt >> copyBitsAgain [
	"Primitive. See BitBlt|copyBits, also a Primitive. Essential. See Object
	documentation whatIsAPrimitive."

	<primitive: 'primitiveCopyBits' module: 'BitBltPlugin'>
	self primitiveFailed
]

{ #category : #private }
BitBlt >> copyBitsFrom: x0 to: x1 at: y [ 
	destX := x0.
	destY := y.
	sourceX := x0.
	width := x1 - x0.
	self copyBits
]

{ #category : #copying }
BitBlt >> copyBitsTranslucent: factor [
	"This entry point to BitBlt supplies an extra argument to specify translucency
	for operations 30 and 31.  The argument must be an integer between 0 and 255."

	<primitive: 'primitiveCopyBits' module: 'BitBltPlugin'>

	"Check for compressed source, destination or halftone forms"
	((sourceForm isForm) and: [sourceForm unhibernate])
		ifTrue: [^ self copyBitsTranslucent: factor].
	((destForm isForm) and: [destForm unhibernate])
		ifTrue: [^ self copyBitsTranslucent: factor].
	((halftoneForm isForm) and: [halftoneForm unhibernate])
		ifTrue: [^ self copyBitsTranslucent: factor].

	self primitiveFailed  "Later do nicer error recovery -- share copyBits recovery"
]

{ #category : #copying }
BitBlt >> copyForm: srcForm to: destPt rule: rule [
	^ self copyForm: srcForm to: destPt rule: rule
		colorMap: (srcForm colormapIfNeededFor: destForm)
]

{ #category : #copying }
BitBlt >> copyForm: srcForm to: destPt rule: rule color: color [ 
	sourceForm := srcForm.
	halftoneForm := color.
	combinationRule := rule.
	destX := destPt x + sourceForm offset x.
	destY := destPt y + sourceForm offset y.
	sourceX := 0.
	sourceY := 0.
	width := sourceForm width.
	height := sourceForm height.
	self copyBits
]

{ #category : #copying }
BitBlt >> copyForm: srcForm to: destPt rule: rule colorMap: map [ 
	sourceForm := srcForm.
	halftoneForm := nil.
	combinationRule := rule.
	destX := destPt x + sourceForm offset x.
	destY := destPt y + sourceForm offset y.
	sourceX := 0.
	sourceY := 0.
	width := sourceForm width.
	height := sourceForm height.
	colorMap := map.
	self copyBits
]

{ #category : #copying }
BitBlt >> copyForm: srcForm to: destPt rule: rule fillColor: color [ 
	sourceForm := srcForm.
	self fillColor: color.	"sets halftoneForm"
	combinationRule := rule.
	destX := destPt x + sourceForm offset x.
	destY := destPt y + sourceForm offset y.
	sourceX := 0.
	sourceY := 0.
	width := sourceForm width.
	height := sourceForm height.
	self copyBits
]

{ #category : #copying }
BitBlt >> copyFrom: sourceRectangle in: srcForm to: destPt [ 
	| sourceOrigin |
	sourceForm := srcForm.
	halftoneForm := nil.
	combinationRule := 3.	"store"
	destX := destPt x.
	destY := destPt y.
	sourceOrigin := sourceRectangle origin.
	sourceX := sourceOrigin x.
	sourceY := sourceOrigin y.
	width := sourceRectangle width.
	height := sourceRectangle height.
	colorMap := srcForm colormapIfNeededFor: destForm.
	self copyBits
]

{ #category : #accessing }
BitBlt >> destForm [
	^ destForm
]

{ #category : #accessing }
BitBlt >> destOrigin: aPoint [ 
	"Set the receiver's destination top left coordinates to be those of the 
	argument, aPoint."
	destX := aPoint x.
	destY := aPoint y
]

{ #category : #accessing }
BitBlt >> destRect [
	"The rectangle we are about to blit to or just blitted to.  "

	^ destX @ destY extent: width @ height
]

{ #category : #accessing }
BitBlt >> destRect: aRectangle [ 
	"Set the receiver's destination form top left coordinates to be the origin of 
	the argument, aRectangle, and set the width and height of the receiver's 
	destination form to be the width and height of aRectangle."
	destX := aRectangle left.
	destY := aRectangle top.
	width := aRectangle width.
	height := aRectangle height
]

{ #category : #accessing }
BitBlt >> destX: anInteger [ 
	"Set the top left x coordinate of the receiver's destination form to be the 
	argument, anInteger."
	destX := anInteger
]

{ #category : #accessing }
BitBlt >> destX: x destY: y width: w height: h [ 
	"Combined init message saves 3 sends from DisplayScanner"
	destX := x.
	destY := y.
	width := w.
	height := h
]

{ #category : #accessing }
BitBlt >> destY: anInteger [ 
	"Set the top left y coordinate of the receiver's destination form to be the 
	argument, anInteger."
	destY := anInteger
]

{ #category : #'line drawing' }
BitBlt >> drawFrom: startPoint to: stopPoint [ 
	
	 ^ self drawFrom: startPoint to: stopPoint withFirstPoint: true
]

{ #category : #'line drawing' }
BitBlt >> drawFrom: startPoint to: stopPoint withFirstPoint: drawFirstPoint [ 
	"Draw a line whose end points are startPoint and stopPoint.
	The line is formed by repeatedly calling copyBits at every
	point along the line.  If drawFirstPoint is false, then omit
	the first point so as not to overstrike at line junctions."
	"Always draw down, or at least left-to-right"
	| offset point1 point2 forwards |
	forwards := (startPoint y = stopPoint y and: [ startPoint x < stopPoint x ]) or: [ startPoint y < stopPoint y ].
	forwards 
		ifTrue: 
			[ point1 := startPoint.
			point2 := stopPoint ]
		ifFalse: 
			[ point1 := stopPoint.
			point2 := startPoint ].
	sourceForm == nil 
		ifTrue: 
			[ destX := point1 x.
			destY := point1 y ]
		ifFalse: 
			[ width := sourceForm width.
			height := sourceForm height.
			offset := sourceForm offset.
			destX := (point1 x + offset x) rounded.
			destY := (point1 y + offset y) rounded ].

	"Note that if not forwards, then the first point is the last and vice versa.
	We agree to always paint stopPoint, and to optionally paint startPoint."
	(drawFirstPoint or: [ forwards == false	"ie this is stopPoint" ]) ifTrue: [ self copyBits ].
	self 
		drawLoopX: (point2 x - point1 x) rounded
		Y: (point2 y - point1 y) rounded.
	(drawFirstPoint or: 
		[ "ie this is stopPoint"
		forwards ]) ifTrue: [ self copyBits ]
]

{ #category : #'line drawing' }
BitBlt >> drawLoopX: xDelta Y: yDelta [ 
	"Primitive. Implements the Bresenham plotting algorithm (IBM Systems
	Journal, Vol. 4 No. 1, 1965). It chooses a principal direction, and
	maintains a potential, P. When P's sign changes, it is time to move in
	the minor direction as well. This particular version does not write the
	first and last points, so that these can be called for as needed in client code.
	Optional. See Object documentation whatIsAPrimitive."
	| dx dy px py P |
	<primitive: 'primitiveDrawLoop' module: 'BitBltPlugin'>
	dx := xDelta sign.
	dy := yDelta sign.
	px := yDelta abs.
	py := xDelta abs.
	"self copyBits."
	py > px 
		ifTrue: 
			[ "more horizontal"
			P := py // 2.
			1 
				to: py
				do: 
					[ :i | 
					destX := destX + dx.
					(P := P - px) < 0 ifTrue: 
						[ destY := destY + dy.
						P := P + py ].
					i < py ifTrue: [ self copyBits ] ] ]
		ifFalse: 
			[ "more vertical"
			P := px // 2.
			1 
				to: px
				do: 
					[ :i | 
					destY := destY + dy.
					(P := P - py) < 0 ifTrue: 
						[ destX := destX + dx.
						P := P + px ].
					i < px ifTrue: [ self copyBits ] ] ]
]

{ #category : #private }
BitBlt >> eraseBits [
	"Perform the erase operation, which puts 0's in the destination
	wherever the source (which is assumed to be just 1 bit deep)
	has a 1.  This requires the colorMap to be set in order to AND
	all 1's into the destFrom pixels regardless of their size."
	| oldMask oldMap |
	oldMask := halftoneForm.
	halftoneForm := nil.
	oldMap := colorMap.
	self colorMap: (Bitmap 
			with: 0
			with: 4294967295).
	combinationRule := Form erase.
	self copyBits.	"Erase the dest wherever the source is 1"
	halftoneForm := oldMask.	"already converted to a Bitmap"
	colorMap := oldMap
]

{ #category : #copying }
BitBlt >> fill: destRect fillColor: grayForm rule: rule [ 
	"Fill with a Color, not a Form."
	sourceForm := nil.
	self fillColor: grayForm.	"sets halftoneForm"
	combinationRule := rule.
	destX := destRect left.
	destY := destRect top.
	sourceX := 0.
	sourceY := 0.
	width := destRect width.
	height := destRect height.
	self copyBits
]

{ #category : #accessing }
BitBlt >> fillColor [
	^ halftoneForm
]

{ #category : #accessing }
BitBlt >> fillColor: aColorOrPattern [ 
	"The destForm will be filled with this color or pattern of colors.  May be an old Color, a new type Color, a Bitmap (see BitBlt comment), a Pattern, or a Form."
	aColorOrPattern == nil ifTrue: 
		[ halftoneForm := nil.
		^ self ].
	destForm == nil ifTrue: [ self error: 'Must set destForm first' ].
	halftoneForm := destForm bitPatternFor: aColorOrPattern
]

{ #category : #private }
BitBlt >> getPluginName [
	"Private. Return the name of the plugin representing BitBlt.
	Used for dynamically switching between different BB representations only."
	^'BitBltPlugin'
]

{ #category : #accessing }
BitBlt >> halftoneForm [
	"Returns the receivers half tone form. See class commment."
	
	^halftoneForm
]

{ #category : #accessing }
BitBlt >> halftoneForm: aBitmap [
	"Sets the receivers half tone form. See class commment."
	
	halftoneForm := aBitmap
	
 
]

{ #category : #accessing }
BitBlt >> height: anInteger [ 
	"Set the receiver's destination form height to be the argument, anInteger."
	height := anInteger
]

{ #category : #private }
BitBlt >> paintBits [
	"Perform the paint operation, which requires two calls to BitBlt."
	| color oldMap saveRule |
	sourceForm depth = 1 ifFalse: 
		[ ^ self error: 'paint operation is only defined for 1-bit deep sourceForms' ].
	saveRule := combinationRule.
	color := halftoneForm.
	halftoneForm := nil.
	oldMap := colorMap.
	"Map 1's to ALL ones, not just one"
	self colorMap: (Bitmap 
			with: 0
			with: 4294967295).
	combinationRule := Form erase.
	self copyBits.	"Erase the dest wherever the source is 1"
	halftoneForm := color.
	combinationRule := Form under.
	self copyBits.	"then OR, with whatever color, into the hole"
	colorMap := oldMap.
	combinationRule := saveRule

	" | dot |
dot := Form dotOfSize: 32.
((BitBlt destForm: Display
		sourceForm: dot
		fillColor: Color lightGray
		combinationRule: Form paint
		destOrigin: Sensor cursorPoint
		sourceOrigin: 0@0
		extent: dot extent
		clipRect: Display boundingBox)
		colorMap: (Bitmap with: 0 with: 16rFFFFFFFF)) copyBits"
]

{ #category : #copying }
BitBlt >> pixelAt: aPoint [ 
	"Assumes this BitBlt has been set up specially (see the init message,
	BitBlt bitPeekerFromForm:.  Returns the pixel at aPoint."
	sourceX := aPoint x.
	sourceY := aPoint y.
	destForm unhibernate.	"before poking"
	destForm bits 
		at: 1
		put: 0.	"Just to be sure"
	self copyBits.
	^ destForm bits at: 1
]

{ #category : #copying }
BitBlt >> pixelAt: aPoint put: pixelValue [ 
	"Assumes this BitBlt has been set up specially (see the init message,
	BitBlt bitPokerToForm:.  Overwrites the pixel at aPoint."
	destX := aPoint x.
	destY := aPoint y.
	sourceForm unhibernate.	"before poking"
	sourceForm bits 
		at: 1
		put: pixelValue.
	self copyBits
	"
| bb |
bb := (BitBlt bitPokerToForm: Display).
[Sensor anyButtonPressed] whileFalse:
	[bb pixelAt: Sensor cursorPoint put: 55]
"
]

{ #category : #private }
BitBlt >> primDisplayString: aString from: startIndex to: stopIndex map: glyphMap xTable: xTable kern: kernDelta [ 
	| ascii |
	<primitive:'primitiveDisplayString' module:'BitBltPlugin'>
	startIndex 
		to: stopIndex
		do: 
			[ :charIndex | 
			ascii := (aString at: charIndex) asciiValue.
			glyphMap ifNotNil: [:map | ascii := map at: ascii +1].
			sourceX := xTable at: ascii + 1.
			width := (xTable at: ascii + 2) - sourceX.
			self copyBits.
			destX := destX + width + kernDelta ]
]

{ #category : #private }
BitBlt >> roundVariables [
	| maxVal minVal |
	maxVal := SmallInteger maxVal.
	minVal := SmallInteger minVal.
	destX := destX asInteger 
		min: maxVal
		max: minVal.
	destY := destY asInteger 
		min: maxVal
		max: minVal.
	width := width asInteger 
		min: maxVal
		max: minVal.
	height := height asInteger 
		min: maxVal
		max: minVal.
	sourceX := sourceX asInteger 
		min: maxVal
		max: minVal.
	sourceY := sourceY asInteger 
		min: maxVal
		max: minVal.
	clipX := clipX asInteger 
		min: maxVal
		max: minVal.
	clipY := clipY asInteger 
		min: maxVal
		max: minVal.
	clipWidth := clipWidth asInteger 
		min: maxVal
		max: minVal.
	clipHeight := clipHeight asInteger 
		min: maxVal
		max: minVal
]

{ #category : #private }
BitBlt >> setDestForm: df [ 
	| bb |
	bb := df boundingBox.
	destForm := df.
	clipX := bb left.
	clipY := bb top.
	clipWidth := bb width.
	clipHeight := bb height
]

{ #category : #private }
BitBlt >> setDestForm: df sourceForm: sf fillColor: hf combinationRule: cr destOrigin: destOrigin sourceOrigin: sourceOrigin extent: extent clipRect: clipRect [ 
	| aPoint |
	destForm := df.
	sourceForm := sf.
	self fillColor: hf.	"sets halftoneForm"
	combinationRule := cr.
	destX := destOrigin x.
	destY := destOrigin y.
	sourceX := sourceOrigin x.
	sourceY := sourceOrigin y.
	width := extent x.
	height := extent y.
	aPoint := clipRect origin.
	clipX := aPoint x.
	clipY := aPoint y.
	aPoint := clipRect corner.
	clipWidth := aPoint x - clipX.
	clipHeight := aPoint y - clipY.
	sourceForm == nil ifFalse: [ colorMap := sourceForm colormapIfNeededFor: destForm ]
]

{ #category : #accessing }
BitBlt >> sourceForm [

	^ sourceForm
]

{ #category : #accessing }
BitBlt >> sourceForm: aForm [ 
	"Set the receiver's source form to be the argument, aForm."
	sourceForm := aForm
]

{ #category : #accessing }
BitBlt >> sourceOrigin: aPoint [ 
	"Set the receiver's source form coordinates to be those of the argument, 
	aPoint."
	sourceX := aPoint x.
	sourceY := aPoint y
]

{ #category : #accessing }
BitBlt >> sourceRect: aRectangle [ 
	"Set the receiver's source form top left x and y, width and height to be 
	the top left coordinate and extent of the argument, aRectangle."
	sourceX := aRectangle left.
	sourceY := aRectangle top.
	width := aRectangle width.
	height := aRectangle height
]

{ #category : #accessing }
BitBlt >> sourceX: anInteger [ 
	"Set the receiver's source form top left x to be the argument, anInteger."
	sourceX := anInteger
]

{ #category : #accessing }
BitBlt >> sourceY: anInteger [ 
	"Set the receiver's source form top left y to be the argument, anInteger."
	sourceY := anInteger
]

{ #category : #accessing }
BitBlt >> tallyMap [
	"Return the map used for tallying pixels"
	^colorMap
]

{ #category : #accessing }
BitBlt >> tallyMap: aBitmap [ 
	"Install the map used for tallying pixels"
	colorMap := aBitmap
]

{ #category : #accessing }
BitBlt >> width: anInteger [ 
	"Set the receiver's destination form width to be the argument, anInteger."
	width := anInteger
]
