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
ScalingCanvas class >> exampleFillRectangle [

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
ScalingCanvas class >> exampleFrameAndFillRectangle [

	^ self privateExampleWithExtent: 10@24 fillColor: Color white scale: 10 drawBlock: [ :canvas |
		{
			1.0@2.0 extent: 3.0@3.0.
			1.1@6.1 extent: 3.1@3.1.
			1.1@10.1 extent: 3.9@3.9.
			1.9@14.9 extent: 3.1@3.1.
			1.9@18.9 extent: 3.9@3.9
		} do: [ :rectangle |
			canvas
				frameAndFillRectangle: rectangle fillColor: Color blue
					borderWidth: 0 borderColor: Color green;
				frameAndFillRectangle: (rectangle translateBy: 5@1) fillColor: (Color blue alpha: 0.5)
					borderWidth: 0 borderColor: Color green ] ]
]

{ #category : 'instance creation' }
ScalingCanvas class >> formCanvas: formCanvas scale: scale [

	^ self new formCanvas: formCanvas scale: scale
]

{ #category : 'class initialization' }
ScalingCanvas class >> initialize [

	| theme icons iconsScale2 |

	FormMapping := IdentityDictionary new.

	theme := Smalltalk ui theme.
	(theme formsForScale: 1) keysAndValuesDo: [ :formName :formScale1 |
		FormMapping at: formScale1 put: ((theme formsForScale: 2) at: formName) ].

	icons := Smalltalk ui icons.
	icons scale = 1 ifFalse: [ ^ self ].
	(iconsScale2 := ThemeIcons named: icons name)
		loadIconsFromUrlUsingScale: 2.
	icons allIconNames do: [ :iconName |
		iconsScale2 icons at: iconName ifPresent: [ :formScale2 |
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
	
	(baseForm := Form extent: extent depth: Display depth)
		fillColor: fillColor.
	drawBlock value: baseForm getCanvas.
	form1 := baseForm scaledToSize: extent * scale.
	
	(form2 := Form extent: extent * scale depth: Display depth)
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

	FreeTypeSettings current forceNonSubPixelDuring: [
		formCanvas transformBy: (MatrixTransform2x3 withScale: scale)
			clippingTo: formCanvas clipRect
			during: aBlock ]
]

{ #category : 'drawing - support' }
ScalingCanvas >> clipBy: aRectangle during: aBlock [

	formCanvas clipBy: (aRectangle scaleBy: scale) during: [ :transformedFormCanvas |
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

	^ self class formCanvas: (formCanvas copyClipRect: (newClipRect scaleBy: scale)) scale: scale
]

{ #category : 'accessing' }
ScalingCanvas >> depth [

	^ formCanvas depth
]

{ #category : 'private' }
ScalingCanvas >> drawString: s from: firstIndex to: lastIndex in: boundsRect font: fontOrNil color: c [

	formCanvas drawString: s from: firstIndex to: lastIndex in: (boundsRect scaleBy: scale)
		font: fontOrNil color: c scale: scale
]

{ #category : 'drawing - text' }
ScalingCanvas >> drawString: s from: firstIndex to: lastIndex in: boundsRect font: fontOrNil color: c underline: underline underlineColor: uc strikethrough: strikethrough strikethroughColor: sc [

	formCanvas drawString: s from: firstIndex to: lastIndex in: (boundsRect scaleBy: scale)
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

{ #category : 'drawing - rectangles' }
ScalingCanvas >> frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor [

	borderWidth = 0 ifTrue: [
		| maxCorner origin corner scaledRectangle |
		maxCorner := (1 << 31 - 1) asPoint - formCanvas origin.
		origin := r origin truncated * scale.
		corner := origin + (r extent truncated * scale).
		scaledRectangle := Rectangle origin: r origin truncated * scale corner: (corner min: maxCorner).
		formCanvas frameAndFillRectangle: scaledRectangle fillColor: fillColor
			borderWidth: borderWidth borderColor: borderColor.
		^ self ].
	super frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor
]

{ #category : 'private' }
ScalingCanvas >> image: aForm at: aPoint sourceRect: sourceRect rule: rule [

	(scale = 2 and: [ FormMapping includesKey: aForm ]) ifTrue: [
		formCanvas image: (FormMapping at: aForm) at: aPoint * scale sourceRect: (sourceRect scaleBy: scale) rule: rule.
		^ self ].
	super image: aForm at: aPoint sourceRect: sourceRect rule: rule
]

{ #category : 'testing' }
ScalingCanvas >> isShadowDrawing [

	^ formCanvas isShadowDrawing
]

{ #category : 'accessing' }
ScalingCanvas >> origin [

	^ formCanvas origin / scale
]

{ #category : 'drawing' }
ScalingCanvas >> paragraph: paragraph bounds: bounds color: c [

	formCanvas paragraph: paragraph bounds: (bounds scaleBy: scale) color: c scale: scale
]

{ #category : 'accessing' }
ScalingCanvas >> scale [

	^ scale
]

{ #category : 'accessing' }
ScalingCanvas >> shadowColor [

	^ formCanvas shadowColor
]

{ #category : 'accessing' }
ScalingCanvas >> shadowColor: aColor [

	formCanvas shadowColor: aColor
]

{ #category : 'drawing - support' }
ScalingCanvas >> transformBy: aDisplayTransform clippingTo: aClipRect during: aBlock smoothing: cellSize [

	aDisplayTransform isPureTranslation ifTrue: [
		aBlock value: (self class
			formCanvas: (formCanvas
				copyOffset: aDisplayTransform offset negated truncated * scale
				clipRect: (aClipRect scaleBy: scale))
			scale: scale).
		^ self ].
	super transformBy: aDisplayTransform clippingTo: aClipRect during: aBlock smoothing: cellSize
]

{ #category : 'other' }
ScalingCanvas >> translateBy: aPoint clippingTo: aRect during: aBlock [

	formCanvas translateBy: aPoint * scale clippingTo: (aRect scaleBy: scale) during: [ :transformedFormCanvas |
		aBlock value: (self class formCanvas: transformedFormCanvas scale: scale) ]
]

{ #category : 'drawing - support' }
ScalingCanvas >> translateBy: delta during: aBlock [

	formCanvas translateBy: delta * scale during: [ :transformedFormCanvas |
		aBlock value: (self class formCanvas: transformedFormCanvas scale: scale) ]
]
