"
A ScalingCanvas provides scaling on an underlying `FormCanvas`.

The method `ScalingCanvas class>>#example` and other methods in the same protocol compare drawing using a FormCanvas and then scaling the Form to drawing using a ScalingCanvas directly on a scaled Form. Using the ScalingCanvas, text is drawn using more detailed character glyphs, and any `FormSet` is drawn using the scale-specific Form if available.
"
Class {
	#name : 'ScalingCanvas',
	#superclass : 'PluggableCanvas',
	#instVars : [
		'formCanvas',
		'scale'
	],
	#classVars : [
		'FormMapping'
	],
	#category : 'Graphics-Canvas-Canvases',
	#package : 'Graphics-Canvas',
	#tag : 'Canvases'
}

{ #category : 'examples' }
ScalingCanvas class >> example [

	| morph |

	(morph := MenuMorph new)
		addTitle: 'Lorem ipsum'.
	(morph add: 'Dolor sit amet' target: nil selector: #yourself) keyText: 'x';
		icon: (self iconNamed: #smallConfiguration).
	(morph add: 'Consectetur adipiscing elit' target: nil selector: #yourself) keyText: 'y'.
	morph addLine.
	(morph add: 'Sed do eiusmod' target: nil selector: #yourself) keyText: 'z'.
	
	^ self privateExampleWithMorph: morph
]

{ #category : 'examples' }
ScalingCanvas class >> exampleDrawPolygon [

	^ self privateExampleWithExtent: 37@44 fillColor: Color white scale: 7 drawBlock: [ :canvas |
		{
			{ 2@2. 2@7. 7@7. 7@2 }.
			{ 8.1@2.1. 8.1@7.1. 13.1@7.1. 13.1@2.1 }.
			{ 14.9@2.9. 14.9@7.9. 19.9@7.9. 19.9@2.9 }.
			{ 2@9. 19@9. 19@15. 11@15. 11@19. 2@19 }.
			{ 2.9@21.1. 19.1@21.1. 19.1@27.9. 11.5@27.9. 11.5@31.3. 2.9@31.3 }.
			{ 18@34. 18@39. 3@39. 3@42. 12@42. 12@34 }.
			{ 25@11. 30@1. 35@11 }.
			{ 35@22. 26@22. 35@13. 26@13 }.
		} do: [ :vertices |
			canvas drawPolygon: vertices fillStyle: (SolidFillStyle color: (Color red alpha: 0.25)) ] ]
]

{ #category : 'examples' }
ScalingCanvas class >> exampleDrawString [

	^ self privateExampleWithExtent: 150@40 fillColor: Color white scale: 4 drawBlock: [ :canvas |
		| bounds |
		bounds := 5.5@3.5 corner: 144.5@37.5.
		canvas
			frameRectangle: bounds color: Color green;
			drawString: 'Lorem ipsum' in: bounds
				font: (LogicalFont familyName: 'Source Sans Pro' pointSize: 20) color: Color blue
				underline: true underlineColor: Color red
				strikethrough: true strikethroughColor: Color red ]
]

{ #category : 'examples' }
ScalingCanvas class >> exampleFillOval [

	^ self privateExampleWithExtent: 10@10 fillColor: Color white scale: 10 drawBlock: [ :canvas |
		canvas fillOval: (1.1@1.1 corner: 9.9@9.9) color: Color blue borderWidth: 2 borderColor: Color green ]
]

{ #category : 'examples' }
ScalingCanvas class >> exampleFillRectangle1 [

	^ self privateExampleWithExtent: 11@3 fillColor: Color white scale: 10 drawBlock: [ :canvas |
		{
			1.0@1.0 extent: 1.0@1.0.
			3.1@1.1 extent: 1.1@1.1.
			5.1@1.1 extent: 1.9@1.9.
			7.9@1.9 extent: 1.1@1.1.
			9.9@1.9 extent: 1.9@1.9
		} do: [ :rectangle |
			canvas fillRectangle: rectangle color: Color blue ] ]
]

{ #category : 'examples' }
ScalingCanvas class >> exampleFillRectangle2 [

	^ self privateExampleWithExtent: 50@50 fillColor: Color white scale: 10 drawBlock: [ :canvas |
		0 to: 3 do: [ :i |
			| colors gradientFillStyle rectangleOrigin |
			colors := { Color red. Color green. Color blue } collect: [ :color | color alpha: 0.25 * (4 - i) ].
			(gradientFillStyle := GradientFillStyle colors: colors)
				origin: 2@0;
				direction: 46@0.
			canvas fillRectangle: (0@(i * 4) extent: 50@4) fillStyle: gradientFillStyle.
			(gradientFillStyle := GradientFillStyle colors: colors)
				origin: 0@48;
				direction: (0@30) negated.
			canvas fillRectangle: ((i * 4)@16 extent: 4@34) fillStyle: gradientFillStyle.
			rectangleOrigin := 16@16 + ((i \\ 2 * 17)@(i // 2 * 17)).
			(gradientFillStyle := GradientFillStyle colors: colors)
				origin: rectangleOrigin + (8@8);
				direction: 10@10;
				radial: true.
			canvas fillRectangle: (rectangleOrigin extent: 17@17) fillStyle: gradientFillStyle ] ]
]

{ #category : 'examples' }
ScalingCanvas class >> exampleFrameAndFillRectangle1 [
	
	^ self privateExampleWithExtent: 80@54 fillColor: Color white scale: 5 drawBlock: [ :canvas |
		canvas
			frameAndFillRectangle: (5@5 extent: 20@20)
				fillColor: Color blue
				borderWidth: 2 borderColor: Color green;
			frameAndFillRectangle: (30@5 extent: 20@20)
				fillColor: Color green
				borderWidth: 3 borderColor: Color transparent;
			frameAndFillRectangle: (55@5 extent: 20@20)
				fillColor: Color transparent
				borderWidth: 5 borderColor: Color blue;
			frameAndFillRectangle: (6@31 corner: 49@49)
				fillColor: Color cyan translucent
				borderWidth: 4 borderColor: Color red translucent;
			frameAndFillRectangle: (31@31 corner: 74@49)
				fillColor: Color blue translucent
				borderWidth: 4 borderColor: Color yellow translucent ]
]

{ #category : 'examples' }
ScalingCanvas class >> exampleFrameAndFillRectangle2 [

	^ self privateExampleWithExtent: 25@35 fillColor: Color white scale: 10 drawBlock: [ :canvas |
		{
			1.0@2.0 extent: 3.0@3.0.
			1.1@6.1 extent: 3.0@3.0.
			1.9@10.9 extent: 3.0@3.0.
			1.1@14.1 extent: 3.1@3.1.
			1.1@18.1 extent: 3.9@3.9.
			1.9@22.9 extent: 3.1@3.1.
			1.9@26.9 extent: 3.9@3.9
		} do: [ :rectangle |
			canvas
				frameAndFillRectangle: rectangle fillColor: Color blue
					borderWidth: 0 borderColor: Color green;
				frameAndFillRectangle: (rectangle translateBy: 5@1) fillColor: Color blue translucent
					borderWidth: 0 borderColor: Color green;
				frameAndFillRectangle: (rectangle translateBy: 10@2) fillColor: Color blue translucent
					borderWidth: 1 borderColor: Color green translucent;
				frameAndFillRectangle: (rectangle translateBy: 15@3) fillColor: Color blue translucent
					borderWidth: 1.1 borderColor: Color green translucent;
				frameAndFillRectangle: (rectangle translateBy: 20@4) fillColor: Color blue translucent
					borderWidth: 1.9 borderColor: Color green translucent ] ]
]

{ #category : 'examples' }
ScalingCanvas class >> exampleImageMorph1 [

	^ self privateExampleWithMorph: (Morph new
		color: Color veryVeryLightGray;
		addMorph: ((ImageMorph withForm: (self iconNamed: #glamorousSearch))
			position: 4@4;
			yourself);
		addMorph: ((ImageMorph withForm: (self iconNamed: #glamorousSearch) copy)
			position: 24@4;
			yourself);
		extent: 44@24;
		yourself)
]

{ #category : 'examples' }
ScalingCanvas class >> exampleImageMorph2 [

	| forms imageMorph |
	
	forms := (1 to: 2) collect: [ :scale |
		(FormCanvas extent: 100 asPoint * scale depth: 1)
			fillOval: ((0 asPoint extent: 100 asPoint * scale) insetBy: 5 * scale) fillStyle: Color green;
			form ].
	(imageMorph := ImageMorph withFormSet: (FormSet forms: forms))
		color: Color blue.
	^ self privateExampleWithMorph: imageMorph
]

{ #category : 'examples' }
ScalingCanvas class >> exampleImageMorph3 [

	| forms imageMorph |
	
	forms := (1 to: 2) collect: [ :scale |
		(FormCanvas extent: 10 asPoint * scale)
			fillOval: ((0 asPoint extent: 10 asPoint * scale) insetBy: 1 * scale) fillStyle: Color black;
			form ].
	(imageMorph := ImageMorph withFormSet: (FormSet forms: forms))
		resize: 200@100.
	^ self privateExampleWithMorph: imageMorph
]

{ #category : 'examples' }
ScalingCanvas class >> exampleLine1 [

	^ self privateExampleWithExtent: 101@101 fillColor: Color white scale: 4 drawBlock: [ :canvas |
		| center innerRadius outerRadius |
		center := 50@50.
		innerRadius := 15.
		outerRadius := 45.
		0 to: 359 by: 10 do: [ :degrees |
			| factors |
			factors := degrees degreeCos @ degrees degreeSin.
			canvas line: center + (innerRadius * factors) rounded
				to: center + (outerRadius * factors) rounded
				width: 1 color: (Color h: degrees s: 1.0 v: 1.0) ] ]
]

{ #category : 'examples' }
ScalingCanvas class >> exampleLine2 [

	^ self privateExampleWithExtent: 61@40 fillColor: Color white scale: 10 drawBlock: [ :canvas |
		{
			{ 1. { (1@1) -> (1@1). (1@3) -> (2@3). (1@5) -> (3@5) } }.
			{ 2. { (6@2) -> (6@2). (6@5) -> (7@5). (6@8) -> (8@8) } }.
			{ 3. { (11@2) -> (11@2). (11@6) -> (12@6). (11@10) -> (13@10) } }.
			{ 4. { (18@3) -> (18@3). (18@8) -> (19@8). (18@13) -> (20@13) } }.
			{ 5. { (25@3) -> (25@3). (25@9) -> (26@9). (25@15) -> (27@15) } }.
			{ 1. { (23.0@20.0) -> (23.0@20.0). (25.0@20.0) -> (25.1@20.0). (27.0@20.0) -> (27.9@20.0) } }.
			{ 1. { (23.9@22.0) -> (24.0@22.0). (25.1@22.0) -> (26.0@22.0). (28.0@22.0) -> (28.0@22.0) } }.
			{ 1.5. { (23.0@24.0) -> (23.0@24.0). (25.1@24.0) -> (26.9@24.0) } }.
		} do: [ :arguments |
			arguments bind: [ :width :pointAssociations |
				pointAssociations do: [ :pointAssociation |
					canvas
						line: pointAssociation key
							to: pointAssociation value
							width: width color: Color blue;
						line: pointAssociation key transposed + (0@9)
							to: pointAssociation value transposed + (0@9)
							width: width color: Color blue;
						line: pointAssociation key + (30@0)
							to: pointAssociation value + (30@0)
							width: width color: Color blue translucent;
						line: pointAssociation key transposed + (30@9)
							to: pointAssociation value transposed + (30@9)
							width: width color: Color blue translucent ] ] ] ]
]

{ #category : 'instance creation' }
ScalingCanvas class >> formCanvas: formCanvas scale: scale [

	^ self new formCanvas: formCanvas scale: scale
]

{ #category : 'class initialization' }
ScalingCanvas class >> initialize [

	| icons iconFormsScale2 |

	FormMapping := IdentityDictionary new.

	icons := Smalltalk ui icons.
	icons scale = 1 ifFalse: [ ^ self ].
	iconFormsScale2 := icons iconsPerScale at: 2 ifAbsent: [ ^ self ].
	icons allIconNames do: [ :iconName |
		iconFormsScale2 at: iconName ifPresent: [ :formScale2 |
			FormMapping at: (icons iconNamed: iconName) put: formScale2 ] ]
]

{ #category : 'private' }
ScalingCanvas class >> privateExampleWithExtent: extent fillColor: fillColor scale: scale drawBlock: drawBlock [

	^ self privateExampleWithExtent: extent fillColor: fillColor scale: scale
		drawBlock: drawBlock
		do: [ :form1 :form2 | { form1. form2 } inspect ]
]

{ #category : 'private' }
ScalingCanvas class >> privateExampleWithExtent: extent fillColor: fillColor scale: scale drawBlock: drawBlock do: doBlock [

	| baseForm form1 form2 |
	
	(baseForm := Form extent: extent depth: 32)
		fillColor: fillColor.
	drawBlock value: baseForm getCanvas.
	form1 := baseForm scaledToSize: extent * scale.
	
	(form2 := Form extent: extent * scale depth: 32)
		fillColor: fillColor.
	drawBlock value: (self formCanvas: form2 getCanvas scale: scale).
	
	^ doBlock value: form1 value: form2
]

{ #category : 'private' }
ScalingCanvas class >> privateExampleWithMorph: morph [

	^ self privateExampleWithExtent: morph fullBounds bottomRight
		fillColor: Smalltalk ui theme backgroundColor
		scale: 2
		drawBlock: [ :canvas | canvas fullDrawMorph: morph ]
		do: [ :form1 :form2 | { morph. form1. form2 } inspect ]
]

{ #category : 'private' }
ScalingCanvas >> allocateForm: extentPoint [

	^ formCanvas allocateForm: extentPoint
]

{ #category : 'private' }
ScalingCanvas >> apply: aBlock [

	self applyClippingTo: self clipRect during: aBlock
]

{ #category : 'private' }
ScalingCanvas >> applyClippingTo: clipRect during: aBlock [

	FreeTypeSettings current forceNonSubPixelDuring: [
		| scaledClipRect |
		scaledClipRect := ((clipRect scaleBy: scale)
			intersect: formCanvas clipRect ifNone: [ 0@0 corner: 0@0 ])
				intersect: (formCanvas origin negated extent: formCanvas extent) ifNone: [ 0@0 corner: 0@0 ].
		self depth = 32 ifTrue: [
			| sourceForm |
			sourceForm := Form extent: scaledClipRect extent / scale depth: 32.
			(FormCanvas on: sourceForm) translateBy: scaledClipRect origin negated / scale during: aBlock.
			(formCanvas warpFrom: sourceForm boundingBox innerCorners toRect: scaledClipRect)
				combinationRule: Form blendAlphaScaled;
				sourceForm: sourceForm;
				warpBits
		] ifFalse: [
			formCanvas transformBy: (MatrixTransform2x3 withScale: scale)
				clippingTo: scaledClipRect
				during: aBlock ] ]
]

{ #category : 'drawing - support' }
ScalingCanvas >> clipBy: aRectangle during: aBlock [

	formCanvas clipBy: (aRectangle floor scaleBy: scale) during: [ :transformedFormCanvas |
		aBlock value: (self class formCanvas: transformedFormCanvas scale: scale) ]
]

{ #category : 'accessing' }
ScalingCanvas >> clipRect [

	^ formCanvas clipRect scaleBy: scale reciprocal
]

{ #category : 'accessing' }
ScalingCanvas >> contentsOfArea: aRectangle into: aForm [

	| contentsForm |

	contentsForm := aForm blankCopyOf: aForm boundingBox scaledBy: scale.
	formCanvas contentsOfArea: (aRectangle scaleBy: scale) into: contentsForm.
	(contentsForm scaledToSize: aForm extent) contentsOfArea: aForm boundingBox into: aForm.
	^ aForm
]

{ #category : 'copying' }
ScalingCanvas >> copyClipRect: newClipRect [

	^ self class formCanvas: (formCanvas copyClipRect: (newClipRect floor scaleBy: scale)) scale: scale
]

{ #category : 'accessing' }
ScalingCanvas >> depth [

	^ formCanvas depth
]

{ #category : 'drawing - images' }
ScalingCanvas >> drawFormSet: formSet at: point [

	formCanvas drawImage: (self scaledFormForFormSet: formSet) at: point * scale
]

{ #category : 'drawing - polygons' }
ScalingCanvas >> drawPolygon: vertices color: aColor borderWidth: bw borderColor: bc [

	bw = 0 ifTrue: [
		(self scalePolygon: vertices) ifNotNil: [ :scaledPolygon |
			formCanvas drawPolygon: scaledPolygon color: aColor borderWidth: 0 borderColor: bc.
			^ self ] ].
	self applyClippingTo: (vertices min floor corner: vertices max floor + 1)
		during: [ :transformedCanvas |
			transformedCanvas drawPolygon: vertices color: aColor borderWidth: bw borderColor: bc ]
]

{ #category : 'drawing - polygons' }
ScalingCanvas >> drawPolygon: vertices fillStyle: aFillStyle borderWidth: bw borderColor: bc [

	aFillStyle isColor ifTrue: [
		self drawPolygon: vertices color: aFillStyle borderWidth: bw borderColor: bc.
		^ self ].
	self applyClippingTo: (vertices min floor corner: vertices max floor + 1)
		during: [ :transformedCanvas |
			transformedCanvas drawPolygon: vertices fillStyle: aFillStyle borderWidth: bw borderColor: bc ]
]

{ #category : 'drawing - text' }
ScalingCanvas >> drawString: s from: firstIndex to: lastIndex in: boundsRect font: fontOrNil color: c [

	formCanvas drawString: s from: firstIndex to: lastIndex in: (boundsRect floor scaleBy: scale)
		font: fontOrNil color: c scale: scale
]

{ #category : 'drawing - text' }
ScalingCanvas >> drawString: s from: firstIndex to: lastIndex in: boundsRect font: fontOrNil color: c underline: underline underlineColor: uc strikethrough: strikethrough strikethroughColor: sc [

	formCanvas drawString: s from: firstIndex to: lastIndex in: (boundsRect floor scaleBy: scale)
		font: fontOrNil color: c
		underline: underline underlineColor: uc
		strikethrough: strikethrough strikethroughColor: sc
		scale: scale
]

{ #category : 'accessing' }
ScalingCanvas >> extent [

	^ formCanvas extent / scale
]

{ #category : 'drawing' }
ScalingCanvas >> fillColor: aColor [

	formCanvas fillColor: aColor
]

{ #category : 'drawing - ovals' }
ScalingCanvas >> fillOval: r color: c borderWidth: borderWidth borderColor: borderColor [

	self applyClippingTo: r floor
		during: [ :clippedCanvas |
			clippedCanvas fillOval: r color: c borderWidth: borderWidth borderColor: borderColor ]
]

{ #category : 'drawing - rectangles' }
ScalingCanvas >> fillRectangle: aRectangle basicFillStyle: aFillStyle [

	aFillStyle isGradientFill ifTrue: [
		formCanvas fillRectangle: (aRectangle floor scaleBy: scale)
			basicFillStyle: (aFillStyle copy
				origin: aFillStyle origin * scale + (scale // 2);
				direction: aFillStyle direction * scale;
				normal: aFillStyle normal * scale;
				yourself).
		^ self ].
	super fillRectangle: aRectangle basicFillStyle: aFillStyle
]

{ #category : 'initialization' }
ScalingCanvas >> finish [

	formCanvas finish
]

{ #category : 'initialization' }
ScalingCanvas >> flush [

	formCanvas flush
]

{ #category : 'accessing' }
ScalingCanvas >> form [

	^ formCanvas form
]

{ #category : 'accessing' }
ScalingCanvas >> formCanvas [

	^ formCanvas
]

{ #category : 'initialization' }
ScalingCanvas >> formCanvas: initialFormCanvas scale: initialScale [

	formCanvas := initialFormCanvas.
	scale := initialScale.

]

{ #category : 'private' }
ScalingCanvas >> formSet: aFormSet at: aPoint sourceRect: sourceRect rule: rule [

	formCanvas image: (self scaledFormForFormSet: aFormSet) at: aPoint truncated * scale
		sourceRect: (sourceRect scaleBy: scale) rule: rule
]

{ #category : 'drawing - rectangles' }
ScalingCanvas >> frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor [

	borderWidth truncated = borderWidth ifTrue: [
		| extent extentTruncated |
		extent := r extent.
		extentTruncated := r extent truncated.
		(borderWidth = 0 or: [ extentTruncated = extent ]) ifTrue: [
			| maxCorner origin corner scaledRectangle |
			maxCorner := (1 << 31 - 1) asPoint - formCanvas origin.
			origin := r origin truncated * scale.
			corner := origin + (extentTruncated * scale).
			scaledRectangle := Rectangle origin: origin corner: (corner min: maxCorner).
			formCanvas frameAndFillRectangle: scaledRectangle fillColor: fillColor
				borderWidth: borderWidth * scale borderColor: borderColor.
			^ self ] ].
	self applyClippingTo: r floor
		during: [ :transformedCanvas |
			transformedCanvas frameAndFillRectangle: r fillColor: fillColor
				borderWidth: borderWidth borderColor: borderColor ]
]

{ #category : 'private' }
ScalingCanvas >> image: aForm at: aPoint sourceRect: sourceRect rule: rule [

	| scaledForm |

	scaledForm := (scale = 2 and: [ FormMapping includesKey: aForm ])
		ifTrue: [ FormMapping at: aForm ]
		ifFalse: [ aForm scaledToSize: aForm extent * scale ].
	formCanvas image: scaledForm at: aPoint truncated * scale sourceRect: (sourceRect scaleBy: scale) rule: rule
]

{ #category : 'testing' }
ScalingCanvas >> isShadowDrawing [

	^ formCanvas isShadowDrawing
]

{ #category : 'drawing' }
ScalingCanvas >> line: pt1 to: pt2 width: w color: c [

	(c isTranslucentButNotTransparent not and: [ (w truncated = w) and: [
		((pt1 x = pt2 x) or: [ pt1 y = pt2 y ]) and: [
			pt1 truncated = pt1 and: [ pt2 truncated = pt2 ] ] ] ]) ifTrue: [
				| offset widthScaled scaledOffset |
				offset := w // 2.
				widthScaled := w * scale.
				scaledOffset := widthScaled // 2.
				formCanvas line: (pt1 - offset) * scale + scaledOffset
					to: (pt2 - offset) * scale + scaledOffset
					width: widthScaled color: c.
				^ self ].
	self applyClippingTo: (((pt1 min: pt2) - (w // 2)) floor corner: ((pt1 max: pt2) + (w // 2)) ceiling + 1)
		during: [ :transformedCanvas |
			transformedCanvas line: pt1 to: pt2 width: w color: c ]
]

{ #category : 'accessing' }
ScalingCanvas >> origin [

	^ formCanvas origin / scale
]

{ #category : 'drawing - images' }
ScalingCanvas >> paintImage: aForm at: aPoint [

	self paintImage: aForm
		at: aPoint
		sourceRect: aForm boundingBox
]

{ #category : 'drawing - images' }
ScalingCanvas >> paintImage: aForm at: aPoint sourceRect: sourceRect [

	^self image: aForm
		at: aPoint
		sourceRect: sourceRect
		rule: Form paint
]

{ #category : 'drawing' }
ScalingCanvas >> paragraph: paragraph bounds: bounds color: c [

	formCanvas paragraph: paragraph bounds: (bounds scaleBy: scale) color: c scale: scale
]

{ #category : 'accessing' }
ScalingCanvas >> scale [

	^ scale
]

{ #category : 'private' }
ScalingCanvas >> scalePolygon: vertices [

	| previousPoint |
	
	vertices ifEmpty: [ ^ #() ].
	previousPoint := vertices last.
	^ vertices collect: [ :point |
		((point x = previousPoint x) or: [ point y = previousPoint y ]) ifFalse: [ ^ nil ].
		previousPoint := point.
		point truncated * scale ]
]

{ #category : 'private' }
ScalingCanvas >> scaledFormForFormSet: formSet [

	scale = 2 ifTrue: [
		formSet mappableFormForScalingCanvas ifNotNil: [ :mappableForm |
			FormMapping at: mappableForm ifPresent: [ :mappedForm | ^ mappedForm ] ] ].
	^ formSet asFormAtScale: scale
]

{ #category : 'accessing' }
ScalingCanvas >> shadowColor [

	^ formCanvas shadowColor
]

{ #category : 'accessing' }
ScalingCanvas >> shadowColor: aColor [

	formCanvas shadowColor: aColor
]

{ #category : 'drawing - images' }
ScalingCanvas >> stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor [

	formCanvas stencil: (stencilForm scaledToSize: stencilForm extent * scale)
		at: aPoint truncated * scale sourceRect: (sourceRect scaleBy: scale) color: aColor
]

{ #category : 'drawing - images' }
ScalingCanvas >> stencilFormSet: stencilFormSet at: aPoint color: aColor [

	formCanvas stencil: (stencilFormSet asFormAtScale: scale)
		at: aPoint truncated * scale color: aColor
]

{ #category : 'drawing - support' }
ScalingCanvas >> transformBy: aDisplayTransform clippingTo: aClipRect during: aBlock smoothing: cellSize [

	aDisplayTransform isPureTranslation ifTrue: [
		aBlock value: (self class
			formCanvas: (formCanvas
				copyOffset: aDisplayTransform offset negated truncated * scale
				clipRect: (aClipRect floor scaleBy: scale))
			scale: scale).
		^ self ].
	self applyClippingTo: aClipRect floor
		during: [ :transformedCanvas |
			transformedCanvas transformBy: aDisplayTransform clippingTo: aClipRect during: aBlock smoothing: cellSize ]
]

{ #category : 'other' }
ScalingCanvas >> translateBy: aPoint clippingTo: aRect during: aBlock [

	formCanvas translateBy: aPoint * scale clippingTo: (aRect floor scaleBy: scale) during: [ :transformedFormCanvas |
		aBlock value: (self class formCanvas: transformedFormCanvas scale: scale) ]
]

{ #category : 'drawing - support' }
ScalingCanvas >> translateBy: delta during: aBlock [

	formCanvas translateBy: delta * scale during: [ :transformedFormCanvas |
		aBlock value: (self class formCanvas: transformedFormCanvas scale: scale) ]
]

{ #category : 'drawing - images' }
ScalingCanvas >> translucentFormSet: formSet at: point [

	formCanvas translucentImage: (self scaledFormForFormSet: formSet) at: point * scale
]
