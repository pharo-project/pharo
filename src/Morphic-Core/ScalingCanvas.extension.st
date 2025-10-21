Extension { #name : 'ScalingCanvas' }

{ #category : '*Morphic-Core' }
ScalingCanvas >> allocateCanvas: extent [

	^ self class formCanvas: (formCanvas allocateCanvas: extent * scale) scale: scale
]

{ #category : '*Morphic-Core' }
ScalingCanvas >> allocateSavedPatch: extent [

	^ ScalingCanvasSavedPatch
		form: (formCanvas allocateForm: extent * scale)
		scale: scale
]

{ #category : '*Morphic-Core' }
ScalingCanvas >> contentsOfArea: aRectangle intoSavedPatch: savedPatch [

	formCanvas contentsOfArea: (aRectangle scaleBy: scale) intoSavedPatch: savedPatch form
]

{ #category : '*Morphic-Core' }
ScalingCanvas >> drawImageOn: aCanvas at: aPoint [

	aCanvas formCanvas drawImage: self form
		at: aPoint * scale
		sourceRect: (self form boundingBox scaleBy: scale)
]

{ #category : '*Morphic-Core' }
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

{ #category : '*Morphic-Core' }
ScalingCanvas class >> exampleImageMorph1 [

	^ self privateExampleWithMorph: (Morph new
		color: Color veryVeryLightGray;
		addMorph: ((ImageMorph withForm: (self iconNamed: #search))
			position: 4@4;
			yourself);
		addMorph: ((ImageMorph withForm: (self iconNamed: #search) copy)
			position: 24@4;
			yourself);
		extent: 44@24;
		yourself)
]

{ #category : '*Morphic-Core' }
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

{ #category : '*Morphic-Core' }
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

{ #category : '*Morphic-Core' }
ScalingCanvas >> numberOfTransparentPixelsForRoundedCorners [

	^ formCanvas numberOfTransparentPixelsForRoundedCorners * scale * scale
]

{ #category : '*Morphic-Core' }
ScalingCanvas >> paintImageFromScalingCanvas: aScalingCanvas at: aPoint [

	formCanvas paintImage: aScalingCanvas form at: aPoint * scale
]

{ #category : '*Morphic-Core' }
ScalingCanvas >> paintImageOn: aCanvas at: aPoint [

	aCanvas paintImageFromScalingCanvas: self at: aPoint
]

{ #category : '*Morphic-Core' }
ScalingCanvas class >> privateExampleWithMorph: morph [

	^ self privateExampleWithExtent: morph fullBounds bottomRight
		fillColor: Smalltalk ui theme backgroundColor
		scale: 2
		drawBlock: [ :canvas | canvas fullDrawMorph: morph ]
		do: [ :form1 :form2 | { morph. form1. form2 } inspect ]
]

{ #category : '*Morphic-Core' }
ScalingCanvas >> restoreSavedPatch: savedPatch at: aPoint [

	formCanvas restoreSavedPatch: savedPatch form at: aPoint * scale
]

{ #category : '*Morphic-Core' }
ScalingCanvas >> shadowDrawingCanvasWithExtent: extent shadowColor: shadowColor [

	^ (self class formCanvas: (FormCanvas on: (Form extent: extent * scale depth: 1)) scale: scale)
		asShadowDrawingCanvas: shadowColor
]
